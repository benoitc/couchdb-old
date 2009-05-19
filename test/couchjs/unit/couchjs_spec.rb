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

RUN_COUCHJS = "#{COUCH_ROOT}/bin/couchjs #{COUCH_ROOT}/share/server/main.js "

def runCouchJS &block
  puts "running couchjs"
  IO.popen("#{RUN_COUCHJS}") do |couchjs|
    while line = couchjs.gets 
      input = block.call line
      couchjs.puts input
    end
  end  
end

commands = %w{}

runCouchJS do |row|
  return %{["reset"]}
  resp = commands.pop
  puts resp
  
end