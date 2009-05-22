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


# run:
# spec test/couchjs/spec/couchjs_spec.rb -f specdoc --color


COUCH_ROOT = "#{File.dirname(__FILE__)}/../../.." unless defined?(COUCH_ROOT)

RUN_COUCHJS = "#{COUCH_ROOT}/src/couchdb/couchjs #{COUCH_ROOT}/share/server/main.js"

require 'open3'
require 'spec'
require 'json'

class CJS
  def self.run trace = false
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
        elsif rj["error"]
          throw rj
        end
      end
      rj
    else
      throw "no response"
    end
  end
end

describe "couchjs" do
  before(:all) do
    # puts `clear`
    `cd #{COUCH_ROOT} && make`
    @js = CJS.run :trace
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
  # it "should reduce"
  # it "should rereduce"
  # it "should rereduce"
  # it "should validate"
  
  # describe "_show" do
  #   before(:all) do
  #     @fun = <<-JS
  #       function(doc, req) {
  #         return [doc.title, doc.body].join(' - ')
  #       }
  #       JS
  #     @js.reset!
  #   end
  #   it "should show" do
  #     @js.rrun(["show_doc", @fun, 
  #       {:title => "Best ever", :body => "Doc body"}])
  #     @js.rgets.should ==
  #         "Best ever - Doc body\n"
  #   end
  # end
  
  describe "basic new list" do
    before(:all) do
      @fun = <<-JS
        function(head, req) {
          sendChunk("bacon")
          sendChunk(req.q);
          sendChunk(head.foo);
          var row = getRow();
          sendChunk(row.key)
          return "tail";
        }
        JS
      @js.reset!
      @js.add_fun(@fun).should == true
    end
    # it "should new list" do
    #   @js.run(["list", {"foo"=>"bar"}, {"q" => "ok"}]).should == {"chunk"=>"bacon"}
    #   @js.jsgets.should == {"chunk"=>"ok"}
    #   @js.jsgets.should == {"chunk"=>"bar"}
    #   @js.run(["list_row", {"key"=>"baz"}]).should == {"chunk"=>"baz"}
    #   @js.jsgets.should == {"body"=>"tail"}
    # end
    # it "should error if it gets a non-row in the middle" do
    #   @js.run(["list", {"foo"=>"bar"}, {"q" => "ok"}]).should == {"chunk"=>"bacon"}
    #   @js.jsgets.should == {"chunk"=>"ok"}
    #   @js.jsgets.should == {"chunk"=>"bar"}
    #   lambda {@js.run(["reset"])}.should raise_error
    # end
  end
  describe "multi-row new list" do
    before(:all) do
      @fun = <<-JS
        function(head, req) {
          sendChunk("bacon")
          var row;
          while(row = getRow()) {
            sendChunk(row.key);        
          };
          return "tail";
        }
        JS
      @js.reset!
      @js.add_fun(@fun).should == true
    end
    # it "should list all rows" do
    #   @js.run(["list", {"foo"=>"bar"}, {"q" => "ok"}]).should == {"chunk"=>"bacon"}
    #   @js.run(["list_row", {"key"=>"baz"}]).should == {"chunk"=>"baz"}
    #   @js.run(["list_row", {"key"=>"foom"}]).should == {"chunk"=>"foom"}
    #   @js.run(["list_tail"]).should == {"body"=>"tail"}
    # end
    # it "should list all rows" do
    #   @js.run(["list", {"foo"=>"bar"}, {"q" => "ok"}]).should == {"chunk"=>"bacon"}
    #   @js.run(["list_row", {"key"=>"baz"}]).should == {"chunk"=>"baz"}
    #   @js.run(["list_row", {"key"=>"foom"}]).should == {"chunk"=>"foom"}
    #   @js.run(["list_tail"]).should == {"body"=>"tail"}
    # end
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
              return('breaking');
            }
          };
        }
        JS
      @js.reset!
      @js.add_fun(@fun).should == true
    end
    # it "should end early" do
    #   @js.run(["list", {"foo"=>"bar"}, {"q" => "ok"}]).should == {"chunk"=>"bacon"}
    #   @js.run(["list_row", {"key"=>"baz"}]).should == {"chunk"=>"baz"}
    #   @js.run(["list_row", {"key"=>"foom"}]).should == {"chunk"=>"foom"}
    #   @js.run(["list_row", {"key"=>"fooz"}]).should == {"chunk"=>"fooz"}
    #   @js.run(["list_row", {"key"=>"foox"}]).should == {"body"=>"breaking"}
    # end
  end
  
  describe "raw list" do
    before(:each) do
      @fun = <<-JS
        function(head, req) {
          sendChunk("first chunk", true);
          sendChunk("second chunk ");
          sendChunk("third chunk\\n");
          var row;
          while(row = getRow()) {
            sendChunk(row.key);        
          };
          return "tail";
        };
        JS
      @js.reset!
      @js.add_fun(@fun).should == true
    end
    it "should should list em" do
      pending
      @js.rrun(["list", {"foo"=>"bar"}, {"q" => "ok"}])
      @js.rgets.should == "first chunk\n"
      @js.rgets.should == "second chunk third chunk\n"
      m = @js.rrun(["list_row", {"key"=>"baz"}])
      m = @js.rrun(["list_row", {"key"=>"bam"}])
      m = @js.rrun(["list_row", {"key"=>"bar"}])
      m = @js.rrun(["list_tail"])
      # pending
      @js.rgets.should == "bazbambartail\n" 
      @js.jsgets.should == {"end" => "tail"}
      @js.reset!
    end
    it "should error if it gets a non-row in the middle" do
      @js.rrun(["list", {"foo"=>"bar"}, {"q" => "ok"}])
      @js.rgets.should == "first chunk\n"
      @js.rgets.should == "second chunk third chunk\n"
      lambda {@js.run(["reset"])}.should raise_error
    end
  end
  
  describe "raw list with headers" do
    before(:each) do
      @fun = <<-JS
        function(head, req) {
          startResponse({"Content-Type" : "text/plain"});
          sendChunk("first chunk", true);
          return "tail";
        };
        JS
      @js.reset!
      @js.add_fun(@fun).should == true
    end
    it "should description" do
      @js.rrun(["list", {"foo"=>"bar"}, {"q" => "ok"}])
      @js.jsgets.should == {"headers" => {"Content-Type"=>"text/plain"}}
      @js.rgets.should == "first chunk\n"
      @js.jsgets.should == {"end" => "tail"}
    end
  end
end




