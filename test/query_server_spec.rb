# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License.  You may obtain a copy
# of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
# License for the specific language governing permissions and limitations under
# the License.


# to run (require ruby and rspec):
# spec test/query_server_spec.rb -f specdoc --color

COUCH_ROOT = "#{File.dirname(__FILE__)}/.." unless defined?(COUCH_ROOT)


require 'open3'
require 'spec'
require 'json'



class OSProcessRunner
  def self.run
    trace = true
    puts "launching #{run_command}" if trace
    if block_given?
      Open3.popen3(run_command) do |jsin, jsout, jserr|
        js = QueryServerRunner.new(jsin, jsout, jserr, trace)
        yield js
      end
    else
      jsin, jsout, jserr = Open3.popen3(run_command)
      QueryServerRunner.new(jsin, jsout, jserr, trace)
    end
  end
  def initialize jsin, jsout, jserr, trace = false
    @qsin = jsin
    @qsout = jsout
    @qserr = jserr
    @trace = trace
  end
  def close
    @qsin.close
    @qsout.close
    @qserr.close
  end
  def reset!
    run(["reset"])
  end
  def add_fun(fun)
    run(["add_fun", fun])
  end
  def get_chunk
    resp = jsgets
    raise "not a chunk" unless resp.first == "chunk"
    return resp[1]
  end
  def run json
    rrun json
    jsgets
  end
  def rrun json
    line = json.to_json
    puts "run: #{line}" if @trace
    @qsin.puts line
  end
  def rgets
    resp = @qsout.gets
    puts "got: #{resp}"  if @trace
    resp
  end
  def jsgets
    resp = rgets
    # err = @qserr.gets
    # puts "err: #{err}" if err
    if resp
      rj = JSON.parse("[#{resp.chomp}]")[0]
      if rj.respond_to?(:[]) && rj.is_a?(Array)
        if rj[0] == "log"
          log = rj[0]
          puts "log: #{log}" #if @trace
          rj = jsgets
        end
      end
      rj
    else
      raise "no response"
    end
  end
end

class QueryServerRunner < OSProcessRunner
  def self.run_command
    "#{COUCH_ROOT}/src/couchdb/couchjs #{COUCH_ROOT}/share/server/main.js"
  end
end

class ExternalRunner < OSProcessRunner
  def self.run_command
    "#{COUCH_ROOT}/src/couchdb/couchjs #{COUCH_ROOT}/share/server/echo.js"
  end
end

describe "query server normal case" do
  before(:all) do
    `cd #{COUCH_ROOT} && make`
    @qs = QueryServerRunner.run
  end
  after(:all) do
    @qs.close
  end
  it "should reset" do
    @qs.run(["reset"]).should == true    
  end
  it "should run map funs" do
    @qs.reset!
    @qs.run(["add_fun", %{function(doc){emit("foo",doc.a); emit("bar",doc.a)}}]).should == true
    @qs.run(["add_fun", %{function(doc){emit("baz",doc.a)}}]).should == true
    rows = @qs.run(["map_doc", {:a => "b"}])
    rows[0][0].should == ["foo", "b"]
    rows[0][1].should == ["bar", "b"]
    rows[1][0].should == ["baz", "b"]
  end
  describe "reduce" do
    before(:all) do
      @fun = <<-JS
        function(keys, values, rereduce) {
          return values.length;
        }
        JS
      @qs.reset!
    end
    it "should reduce" do
      kvs = (0...10).collect{|i|[i,i*2]}
      @qs.run(["reduce", [@fun], kvs]).should == [true, [10]]
    end
  end
  describe "rereduce" do
    before(:all) do
      @fun = <<-JS
        function(keys, values, rereduce) {
          return sum(values);
        }
        JS
      @qs.reset!
    end
    it "should rereduce" do
      vs = (0...10).collect{|i|i}
      @qs.run(["rereduce", [@fun], vs]).should == [true, [45]]
    end
  end
  
  # it "should validate"
  describe "validation" do
    before(:all) do
      @fun = <<-JS
        function(newDoc, oldDoc, userCtx) {
          if (newDoc.bad) throw({forbidden:"bad doc"});
          "foo bar";
        }
        JS
      @qs.reset!
    end
    it "should allow good updates" do
      @qs.run(["validate", @fun, {"good" => true}, {}, {}]).should == 1
    end
    it "should reject invalid updates" do
      @qs.run(["validate", @fun, {"bad" => true}, {}, {}]).should == {"forbidden"=>"bad doc"}
    end
  end
  
  describe "show" do
    before(:all) do
      @fun = <<-JS
        function(doc, req) {
          log("ok");
          return [doc.title, doc.body].join(' - ')
        }
        JS
      @qs.reset!
    end
    it "should show" do
      @qs.rrun(["show", @fun, 
        {:title => "Best ever", :body => "Doc body"}])
      @qs.jsgets.should == ["end", "Best ever - Doc body"]
    end
  end
  
  describe "show with headers" do
    before(:all) do
      @fun = <<-JS
        function(doc, req) {
          sendHeaders({"X-Plankton":"Rusty"});
          return [doc.title, doc.body].join(' - ')
        }
        JS
      @qs.reset!
    end
    it "should show headers" do
      @qs.rrun(["show", @fun, 
        {:title => "Best ever", :body => "Doc body"}])
      @qs.jsgets.should == ["headers", {"X-Plankton"=>"Rusty"}]
      @qs.jsgets.should == ["end", "Best ever - Doc body"]
    end
  end
    
  describe "raw list with headers" do
    before(:each) do
      @fun = <<-JS
        function(head, req) {
          sendHeaders({"Content-Type" : "text/plain"});
          sendChunk("first chunk");
          sendChunk('second "chunk"');
          return "tail";
        };
        JS
      @qs.reset!
      @qs.add_fun(@fun).should == true
    end
    it "should description" do
      @qs.rrun(["list", {"total_rows"=>1000}, {"q" => "ok"}])
      @qs.jsgets.should == ["headers", {"Content-Type"=>"text/plain"}]
      @qs.jsgets.should == ["chunk", "first chunk"]
      @qs.jsgets.should == ["chunk", 'second "chunk"']
      @qs.jsgets.should == ["end", "tail"]
    end
  end
  
  describe "list with rows" do
    before(:each) do
      @fun = <<-JS
        function(head, req) {
          sendChunk("first chunk");
          sendChunk(req.q);
          var row;
          while(row = getRow()) {
            sendChunk(row.key);        
          };
          return "tail";
        };
        JS
      @qs.run(["reset"]).should == true    
      @qs.add_fun(@fun).should == true
    end
    it "should should list em" do
      @qs.rrun(["list", {"foo"=>"bar"}, {"q" => "ok"}])
      @qs.get_chunk.should == "first chunk"
      @qs.get_chunk.should == "ok"
      @qs.rrun(["list_row", {"key"=>"baz"}])
      @qs.get_chunk.should == "baz"
      @qs.rrun(["list_row", {"key"=>"bam"}])
      @qs.get_chunk.should == "bam"
      @qs.rrun(["list_end"])
      @qs.jsgets.should == ["end", "tail"]
    end
    it "should work with zero rows" do
      @qs.rrun(["list", {"foo"=>"bar"}, {"q" => "ok"}])
      @qs.get_chunk.should == "first chunk"
      @qs.get_chunk.should == "ok"
      @qs.rrun(["list_end"])
      @qs.jsgets.should == ["end", "tail"]
    end
  end

  describe "only goes to 2 list" do
    before(:all) do
      @fun = <<-JS
        function(head, req) {
          sendChunk("bacon")
          var row, i = 0;
          while(row = getRow()) {
            sendChunk(row.key);        
            i += 1;
            if (i > 2) {
              return('early');
            }
          };
        }
        JS
      @qs.reset!
      @qs.add_fun(@fun).should == true
    end
    it "should end early" do
      @qs.run(["list", {"foo"=>"bar"}, {"q" => "ok"}]).should == ["chunk", "bacon"]
      @qs.run(["list_row", {"key"=>"baz"}]).should ==  ["chunk", "baz"]
      @qs.run(["list_row", {"key"=>"foom"}]).should == ["chunk", "foom"]
      @qs.run(["list_row", {"key"=>"fooz"}]).should == ["chunk", "fooz"]
      @qs.run(["list_row", {"key"=>"foox"}]).should == ["end" , "early"]
    end
  end
end

describe "query server that exits" do
  before(:each) do
    @qs = QueryServerRunner.run
  end
  after(:each) do
    @qs.close
  end
  
  describe "only goes to 2 list" do
    before(:each) do
      @fun = <<-JS
        function(head, req) {
          sendChunk("bacon")
          var row, i = 0;
          while(row = getRow()) {
            sendChunk(row.key);        
            i += 1;
            if (i > 2) {
              return('early');
            }
          };
        }
        JS
      @qs.reset!
      @qs.add_fun(@fun).should == true
    end
    it "should exit if erlang sends too many rows" do
      @qs.run(["list", {"foo"=>"bar"}, {"q" => "ok"}]).should == ["chunk", "bacon"]
      @qs.run(["list_row", {"key"=>"baz"}]).should ==  ["chunk", "baz"]
      @qs.run(["list_row", {"key"=>"foom"}]).should == ["chunk", "foom"]
      @qs.run(["list_row", {"key"=>"fooz"}]).should == ["chunk", "fooz"]
      @qs.run(["list_row", {"key"=>"foox"}]).should == ["end" , "early"]
      @qs.rrun(["list_row", {"key"=>"woox"}])
      @qs.jsgets["error"].should == "query_server_error"
      begin
        @qs.run(["reset"])
        "raise before this".should == true
      rescue RuntimeError => e
        e.message.should == "no response"
      rescue Errno::EPIPE
        true.should == true
      end
    end
  end
  
  describe "raw list" do
    before(:each) do
      @fun = <<-JS
        function(head, req) {
          sendChunk("first chunk");
          sendChunk(req.q);
          var row;
          while(row = getRow()) {
            sendChunk(row.key);        
          };
          return "tail";
        };
        JS
      @qs.run(["reset"]).should == true    
      @qs.add_fun(@fun).should == true
    end
    it "should exit if it gets a non-row in the middle" do
      @qs.rrun(["list", {"foo"=>"bar"}, {"q" => "ok"}])
      @qs.get_chunk.should == "first chunk"
      @qs.get_chunk.should == "ok"
      @qs.run(["reset"])["error"].should == "query_server_error"
      begin
        @qs.run(["reset"])
        "raise before this".should == true
      rescue RuntimeError => e
        e.message.should == "no response"
      rescue Errno::EPIPE
        true.should == true
      end
    end
  end  
end

# # tests for the generic "echo" external
# 
# describe "running an external" do
#   before(:all) do
#     @ext = ExternalRunner.run
#     
#   end
#   it "should respond to 'info'" do
#     @ext.rrun(['info'])
#     @ext.jsgets.should == ["info", "echo", "external server that prints its arguments as JSON"]
#   end
#   it "should echo the request" do
#     req_obj = {
#       "body"=>"undefined", 
#       "verb"=>"GET", 
#       "info"=>{
#         "disk_format_version"=>2, 
#         "purge_seq"=>0, 
#         "doc_count"=>9082, 
#         "instance_start_time"=>"1243713611467271", 
#         "update_seq"=>9512, 
#         "disk_size"=>27541604, 
#         "compact_running"=>false, 
#         "db_name"=>"toast", 
#         "doc_del_count"=>1
#       }, 
#       "cookie"=>{}, 
#       "form"=>{}, 
#       "query"=>{"q"=>"stuff"}, 
#       "path"=>["toast", "_ext"], 
#       "headers"=>{
#         "User-Agent"=>"curl/7.18.1 (i386-apple-darwin9.2.2) libcurl/7.18.1 zlib/1.2.3", 
#         "Host"=>"localhost:5984", 
#         "Accept"=>"*/*"
#       }
#     }
#     @ext.rrun(['req', req_obj])
#     @ext.jsgets.should == ["x"]
#   end
# end

