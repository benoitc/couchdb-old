// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License.  You may obtain a copy
// of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
// License for the specific language governing permissions and limitations under
// the License.

couchTests.rewrite = function(debug) {

  var db = new CouchDB("test_suite_db");
  db.deleteDb();
  db.createDb();
  if (debug) debugger;

  var designDoc = {
    _id:"_design/rewrite_test",
    language: "javascript",
    views: {
      test: {
        map: "function (doc) { emit(null, null) }"
      }
    },
    rewrites: [{
      match: ["any_view/<view>"],
      rewrite: ["_design/rewrite_test/_view/<view>"]
    },{
      match: ["any_doc/<doc_id>/<*>"],
      rewrite: ["<doc_id>/<*>"]
    },{
      match: ["design_doc/<name>/<*>"],
      rewrite: ["_design/<name>/<*>"]
    }]
  };

  T(db.save(designDoc).ok);
  T(db.save({_id: "foo"}).ok);
  T(db.save({_id: "foo/bar"}).ok);
 
  var prefix = "/test_suite_db/_design/rewrite_test/_rewrite/"; 

  // Test any_view rewrite rule
  var xhr = CouchDB.request("GET", prefix+"any_view/test");
  T(xhr.status == 200);
  T(JSON.parse(xhr.responseText).total_rows == 2);

  // Test any_doc rewrite rule
  var xhr = CouchDB.request("GET", prefix+"any_doc/foo");
  T(xhr.status == 200);
  var xhr = CouchDB.request("GET", prefix+"any_doc/foo%2Fbar");
  T(xhr.status == 200);

  // Test design_doc rewrite rule
  var xhr = CouchDB.request("GET", prefix+"design_doc/rewrite_test/_view/test");
  T(xhr.status == 200);
  T(JSON.parse(xhr.responseText).total_rows == 2);
};
