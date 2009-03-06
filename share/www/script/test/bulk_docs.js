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

couchTests.bulk_docs = function(debug) {
  var db = new CouchDB("test_suite_db");
  db.deleteDb();
  db.createDb();
  if (debug) debugger;

  var docs = makeDocs(5);

  // Create the docs
  var results = db.bulkSave(docs);
  
  T(results.length == 5);
  for (var i = 0; i < 5; i++) {
    T(results[i].id == docs[i]._id);
    T(results[i].rev);
    // Update the doc
    docs[i].string = docs[i].string + ".00";
  }

  // Save the docs
  results = db.bulkSave(docs);
  T(results.length == 5);
  for (i = 0; i < 5; i++) {
    T(results[i].id == i.toString());
    
    // set the delete flag to delete the docs in the next step
    docs[i]._deleted = true;
  }
  
  // now test a bulk update with a conflict
  
  var doc = db.open("0");
  db.save(doc);

  // Delete the docs
  results = db.bulkSave(docs);
  T(results.length == 5);
  T(results[0].id == "0");
  T(results[0].rev === undefined);
  T(results[0].error == "conflict");
  
  for (i = 1; i < 5; i++) {
    T(results[i].id == i.toString());
    T(results[i].rev)
    T(db.open(docs[i]._id) == null);
  }
  
  // verify creating a document with no id returns a new id
  var req = CouchDB.request("POST", "/test_suite_db/_bulk_docs", {
    body: JSON.stringify({"docs": [{"foo":"bar"}]})
  });
  results = JSON.parse(req.responseText);
  
  T(results[0].id != "");
  T(results[0].rev != "");
};
