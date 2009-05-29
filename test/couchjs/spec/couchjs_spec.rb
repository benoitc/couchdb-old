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


# to run:
# spec test/couchjs/spec/couchjs_spec.rb -f specdoc --color

COUCH_ROOT = "#{File.dirname(__FILE__)}/../../.." unless defined?(COUCH_ROOT)

RUN_COUCHJS = "#{COUCH_ROOT}/src/couchdb/couchjs #{COUCH_ROOT}/share/server/main.js"

require 'open3'
require 'spec'
require 'json'

class CJS
  def self.run
    trace = false
    puts "launching #{RUN_COUCHJS}"
    if block_given?
      Open3.popen3(RUN_COUCHJS) do |jsin, jsout, jserr|
        js = CJS.new(jsin, jsout, jserr, trace)
        yield js
      end
    else
      jsin, jsout, jserr = Open3.popen3(RUN_COUCHJS)
      CJS.new(jsin, jsout, jserr, trace)
    end
  end
  def initialize jsin, jsout, jserr, trace = false
    @jsin = jsin
    @jsout = jsout
    @jserr = jserr
    @trace = trace
  end
  def close
    @jsin.close
    @jsout.close
    @jserr.close
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
    @jsin.puts line
  end
  def rgets
    resp = @jsout.gets
    puts "got: #{resp}"  if @trace
    resp
  end
  def jsgets
    resp = rgets
    # err = @jserr.gets
    # puts "err: #{err}" if err
    if resp
      rj = JSON.parse("[#{resp.chomp}]")[0]
      if rj.respond_to?(:[]) && !rj.is_a?(Array) && 
        if rj["log"]
          log = rj["log"]
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

describe "couchjs normal case" do
  before(:all) do
    `cd #{COUCH_ROOT} && make`
    @js = CJS.run
  end
  after(:all) do
    @js.close
  end
  it "should reset" do
    @js.run(["reset"]).should == true    
  end
  it "should run map funs" do
    @js.reset!
    @js.run(["add_fun", %{function(doc){emit("foo",doc.a); emit("bar",doc.a)}}]).should == true
    @js.run(["add_fun", %{function(doc){emit("baz",doc.a)}}]).should == true
    rows = @js.run(["map_doc", {:a => "b"}])
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
      @js.reset!
    end
    it "should reduce" do
      kvs = (0...10).collect{|i|[i,i*2]}
      @js.run(["reduce", [@fun], kvs]).should == [true, [10]]
    end
  end
  describe "rereduce" do
    before(:all) do
      @fun = <<-JS
        function(keys, values, rereduce) {
          return sum(values);
        }
        JS
      @js.reset!
    end
    it "should rereduce" do
      vs = (0...10).collect{|i|i}
      @js.run(["rereduce", [@fun], vs]).should == [true, [45]]
    end
  end
  
  # it "should validate"
  
  describe "show" do
    before(:all) do
      @fun = <<-JS
        function(doc, req) {
          return [doc.title, doc.body].join(' - ')
        }
        JS
      @js.reset!
    end
    it "should show" do
      @js.rrun(["show", @fun, 
        {:title => "Best ever", :body => "Doc body"}])
      @js.jsgets.should == ["end", "Best ever - Doc body"]
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
      @js.reset!
    end
    it "should show" do
      @js.rrun(["show", @fun, 
        {:title => "Best ever", :body => "Doc body"}])
      @js.jsgets.should == ["headers", {"X-Plankton"=>"Rusty"}]
      @js.jsgets.should == ["end", "Best ever - Doc body"]
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
      @js.reset!
      @js.add_fun(@fun).should == true
    end
    it "should description" do
      @js.rrun(["list", {"total_rows"=>1000}, {"q" => "ok"}])
      @js.jsgets.should == ["headers", {"Content-Type"=>"text/plain"}]
      @js.jsgets.should == ["chunk", "first chunk"]
      @js.jsgets.should == ["chunk", 'second "chunk"']
      @js.jsgets.should == ["end", "tail"]
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
      @js.run(["reset"]).should == true    
      @js.add_fun(@fun).should == true
    end
    it "should should list em" do
      @js.rrun(["list", {"foo"=>"bar"}, {"q" => "ok"}])
      @js.get_chunk.should == "first chunk"
      @js.get_chunk.should == "ok"
      @js.rrun(["list_row", {"key"=>"baz"}])
      @js.get_chunk.should == "baz"
      @js.rrun(["list_row", {"key"=>"bam"}])
      @js.get_chunk.should == "bam"
      @js.rrun(["list_end"])
      @js.jsgets.should == ["end", "tail"]
    end
    it "should work with zero rows" do
      @js.rrun(["list", {"foo"=>"bar"}, {"q" => "ok"}])
      @js.get_chunk.should == "first chunk"
      @js.get_chunk.should == "ok"
      @js.rrun(["list_end"])
      @js.jsgets.should == ["end", "tail"]
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
      @js.reset!
      @js.add_fun(@fun).should == true
    end
    it "should end early" do
      @js.run(["list", {"foo"=>"bar"}, {"q" => "ok"}]).should == ["chunk", "bacon"]
      @js.run(["list_row", {"key"=>"baz"}]).should ==  ["chunk", "baz"]
      @js.run(["list_row", {"key"=>"foom"}]).should == ["chunk", "foom"]
      @js.run(["list_row", {"key"=>"fooz"}]).should == ["chunk", "fooz"]
      @js.run(["list_row", {"key"=>"foox"}]).should == ["end" , "early"]
    end
  end
end

describe "couchjs exiting" do
  before(:each) do
    @js = CJS.run
  end
  after(:each) do
    @js.close
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
      @js.reset!
      @js.add_fun(@fun).should == true
    end
    it "should exit if erlang sends too many rows" do
      @js.run(["list", {"foo"=>"bar"}, {"q" => "ok"}]).should == ["chunk", "bacon"]
      @js.run(["list_row", {"key"=>"baz"}]).should ==  ["chunk", "baz"]
      @js.run(["list_row", {"key"=>"foom"}]).should == ["chunk", "foom"]
      @js.run(["list_row", {"key"=>"fooz"}]).should == ["chunk", "fooz"]
      @js.run(["list_row", {"key"=>"foox"}]).should == ["end" , "early"]
      @js.rrun(["list_row", {"key"=>"woox"}])
      @js.jsgets["error"].should == "query_server_error"
      begin
        @js.run(["reset"])
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
      @js.run(["reset"]).should == true    
      @js.add_fun(@fun).should == true
    end
    it "should exit if it gets a non-row in the middle" do
      @js.rrun(["list", {"foo"=>"bar"}, {"q" => "ok"}])
      @js.get_chunk.should == "first chunk"
      @js.get_chunk.should == "ok"
      @js.run(["reset"])["error"].should == "query_server_error"
      begin
        @js.run(["reset"])
        "raise before this".should == true
      rescue RuntimeError => e
        e.message.should == "no response"
      rescue Errno::EPIPE
        true.should == true
      end
    end
  end  
end


