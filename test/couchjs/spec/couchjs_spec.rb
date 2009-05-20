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


COUCH_ROOT = "#{File.dirname(__FILE__)}/../../.." unless defined?(COUCH_ROOT)

RUN_COUCHJS = "#{COUCH_ROOT}/bin/couchjs #{COUCH_ROOT}/share/server/main.js"

require 'open3'
require 'spec'
require 'json'

class CJS
  def self.run
    # puts "launching #{RUN_COUCHJS}"
    if block_given?
      Open3.popen3(RUN_COUCHJS) do |jsin, jsout, jserr|
        js = CJS.new(jsin, jsout, jserr)
        yield js
      end
    else
      jsin, jsout, jserr = Open3.popen3(RUN_COUCHJS)
      CJS.new(jsin, jsout, jserr)
    end
  end
  def initialize jsin, jsout, jserr
    @jsin = jsin
    @jsout = jsout
    @jserr = jserr
  end
  def close
    @jsin.close
    @jsout.close
    @jserr.close
  end
  def reset!
    r(["reset"])
  end
  def add_fun(fun)
    r(["add_fun", fun])
  end
  def r json
    line = json.to_json
    # puts "run: #{line}"
    @jsin.puts line
    resp = @jsout.gets
    # err = @jserr.gets
    # puts "err: #{err}" if err
    if resp
      # puts "got: #{resp}"
      JSON.parse("[#{resp.chomp}]")[0]
    else
      throw "error"
    end
  end
end

describe "couchjs" do
  before(:all) do
    @js = CJS.run
  end
  after(:all) do
    @js.close
  end
  it "should reset" do
    @js.r(["reset"]).should == true    
  end
  it "should run map funs" do
    @js.reset!
    @js.r(["add_fun", %{function(doc){emit("foo",doc.a); emit("bar",doc.a)}}]).should == true
    @js.r(["add_fun", %{function(doc){emit("baz",doc.a)}}]).should == true
    rows = @js.r(["map_doc", {:a => "b"}])
    rows[0][0].should == ["foo", "b"]
    rows[0][1].should == ["bar", "b"]
    rows[1][0].should == ["baz", "b"]
  end
  it "should reduce"
  it "should rereduce"
  it "should rereduce"
  it "should validate"
  describe "_show" do
    before(:all) do
      @fun = <<-JS
        function(doc, req) {
          return [doc.title, doc.body].join(' - ')
        }
        JS
      @js.reset!
    end
    it "should show" do
      @js.r(["show_doc", @fun, 
        {:title => "Best ever", :body => "Doc body"}])["body"].should ==
          "Best ever - Doc body"
    end
  end
  describe "_list" do
    before(:all) do
      @fun = <<-JS
        function(doc, req) {
          return [doc.title, doc.body].join(' - ')
        }
        JS
      @js.reset!
      @js.add_fun(@fun).should == true
    end
    it "should list" do
    end
  end
end




