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

// Used by replication test
CouchDB.host = (typeof window == 'undefined' || !window) ? 
                  "127.0.0.1:5984" : window.location.host;

var tests = {

  // Do some basic tests.
  basics: function(debug) {
    var result = JSON.parse(CouchDB.request("GET", "/").responseText);
    T(result.couchdb == "Welcome"); 
    
    var db = new CouchDB("test_suite_db");
    db.deleteDb();

    // bug COUCHDB-100: DELETE on non-existent DB returns 500 instead of 404
    db.deleteDb();
    
    db.createDb();

    // PUT on existing DB should return 409 instead of 500
    xhr = CouchDB.request("PUT", "/test_suite_db/");
    T(xhr.status == 409);
    if (debug) debugger;

    // Get the database info, check the db_name
    T(db.info().db_name == "test_suite_db");

    // Get the database info, check the doc_count
    T(db.info().doc_count == 0);

    // create a document and save it to the database
    var doc = {_id:"0",a:1,b:1};
    var result = db.save(doc);

    T(result.ok==true); // return object has an ok member with a value true
    T(result.id); // the _id of the document is set.
    T(result.rev); // the revision id of the document is set.

    // Verify the input doc is now set with the doc id and rev
    // (for caller convenience).
    T(doc._id == result.id && doc._rev == result.rev);

    var id = result.id; // save off the id for later

    // make sure the revs_info status is good
    var doc = db.open(id, {revs_info:true});
    T(doc._revs_info[0].status == "available");

    // Create some more documents.
    // Notice the use of the ok member on the return result.
    T(db.save({_id:"1",a:2,b:4}).ok);
    T(db.save({_id:"2",a:3,b:9}).ok);
    T(db.save({_id:"3",a:4,b:16}).ok);

    // Check the database doc count
    T(db.info().doc_count == 4);

    // Test a simple map functions

    // create a map function that selects all documents whose "a" member
    // has a value of 4, and then returns the document's b value.
    var mapFunction = function(doc){
      if (doc.a==4)
        emit(null, doc.b);
    };

    results = db.query(mapFunction);

    // verify only one document found and the result value (doc.b).
    T(results.total_rows == 1 && results.rows[0].value == 16);

    // reopen document we saved earlier
    existingDoc = db.open(id);

    T(existingDoc.a==1);

    //modify and save
    existingDoc.a=4;
    db.save(existingDoc);

    // redo the map query
    results = db.query(mapFunction);

    // the modified document should now be in the results.
    T(results.total_rows == 2);

    // write 2 more documents
    T(db.save({a:3,b:9}).ok);
    T(db.save({a:4,b:16}).ok);

    results = db.query(mapFunction);

    // 1 more document should now be in the result.
    T(results.total_rows == 3);
    T(db.info().doc_count == 6);

    var reduceFunction = function(keys, values){
      return sum(values);
    };

    results = db.query(mapFunction, reduceFunction);

    T(results.rows[0].value == 33);

    // delete a document
    T(db.deleteDoc(existingDoc).ok);

    // make sure we can't open the doc
    T(db.open(existingDoc._id) == null);

    results = db.query(mapFunction);

    // 1 less document should now be in the results.
    T(results.total_rows == 2);
    T(db.info().doc_count == 5);

    // make sure we can still open the old rev of the deleted doc
    T(db.open(existingDoc._id, {rev: existingDoc._rev}) != null);
    
    // make sure restart works
    T(db.ensureFullCommit().ok);
    restartServer();
    
    // make sure we can still open
    T(db.open(existingDoc._id, {rev: existingDoc._rev}) != null);
  },
  
  delayed_commits: function(debug) {
    var db = new CouchDB("test_suite_db");
    db.deleteDb();
    db.createDb();
    if (debug) debugger;
    
    // By default, couchdb doesn't fully commit documents to disk right away,
    // it waits about a second to batch the full commit flush along with any 
    // other updates. If it crashes or is restarted you may lose the most
    // recent commits.
    
    T(db.save({_id:"1",a:2,b:4}).ok);
    T(db.open("1") != null);
    
    restartServer();
    
    T(db.open("1") == null); // lost the update.
    // note if we waited > 1 sec before the restart, the doc would likely
    // commit.
    
    
    // Retry the same thing but with full commits on.
    
    var db2 = new CouchDB("test_suite_db", {"X-Couch-Full-Commit":"true"});
    
    T(db2.save({_id:"1",a:2,b:4}).ok);
    T(db2.open("1") != null);
    
    restartServer();
    
    T(db2.open("1") != null);
    
    // You can update but without committing immediately, and then ensure
    // everything is commited in the last step.
    
    T(db.save({_id:"2",a:2,b:4}).ok);
    T(db.open("2") != null);
    T(db.ensureFullCommit().ok);
    restartServer();
    
    T(db.open("2") != null);
    
    // However, it's possible even when flushed, that the server crashed between
    // the update and the commit, and you don't want to check to make sure
    // every doc you updated actually made it to disk. So record the instance
    // start time of the database before the updates and then check it again
    // after the flush (the instance start time is returned by the flush
    // operation). if they are the same, we know everything was updated
    // safely.
    
    // First try it with a crash.
    
    var instanceStartTime = db.info().instance_start_time;
    
    T(db.save({_id:"3",a:2,b:4}).ok);
    T(db.open("3") != null);
    
    restartServer();
    
    var commitResult = db.ensureFullCommit();
    T(commitResult.ok && commitResult.instance_start_time != instanceStartTime);
    // start times don't match, meaning the server lost our change
    
    T(db.open("3") == null); // yup lost it
    
    // retry with no server restart
    
    var instanceStartTime = db.info().instance_start_time;
    
    T(db.save({_id:"4",a:2,b:4}).ok);
    T(db.open("4") != null);
    
    var commitResult = db.ensureFullCommit();
    T(commitResult.ok && commitResult.instance_start_time == instanceStartTime);
    // Successful commit, start times match!
    
    restartServer();
    
    T(db.open("4") != null);
    
  },
  
  all_docs: function(debug) {
    var db = new CouchDB("test_suite_db");
    db.deleteDb();
    db.createDb();
    if (debug) debugger;
    
    // Create some more documents.
    // Notice the use of the ok member on the return result.
    T(db.save({_id:"0",a:1,b:1}).ok);
    T(db.save({_id:"3",a:4,b:16}).ok);
    T(db.save({_id:"1",a:2,b:4}).ok);
    T(db.save({_id:"2",a:3,b:9}).ok);

    // Check the all docs
    var results = db.allDocs();
    var rows = results.rows;

    T(results.total_rows == results.rows.length);

    for(var i=0; i < rows.length; i++) {
      T(rows[i].id >= "0" && rows[i].id <= "4");
    }
    
    // Check _all_docs with descending=true
    var desc = db.allDocs({descending:true});
    T(desc.total_rows == desc.rows.length);
 
    // Check _all_docs offset
    var all = db.allDocs({startkey:"2"});
    T(all.offset == 2);
    
    // check that the docs show up in the seq view in the order they were created
    var all_seq = db.allDocsBySeq();
    var ids = ["0","3","1","2"];
    for (var i=0; i < all_seq.rows.length; i++) {
      var row = all_seq.rows[i];
      T(row.id == ids[i]);
    };
    
    // it should work in reverse as well
    all_seq = db.allDocsBySeq({descending:true});
    ids = ["2","1","3","0"];
    for (var i=0; i < all_seq.rows.length; i++) {
      var row = all_seq.rows[i];
      T(row.id == ids[i]);
    };
    
    // check that deletions also show up right
    var doc1 = db.open("1");
    var deleted = db.deleteDoc(doc1);
    T(deleted.ok);
    all_seq = db.allDocsBySeq();
    
    // the deletion should make doc id 1 have the last seq num
    T(all_seq.rows.length == 4);
    T(all_seq.rows[3].id == "1");
    T(all_seq.rows[3].value.deleted);

    // is this a bug?
    // T(all_seq.rows.length == all_seq.total_rows);
    
    // do an update
    var doc2 = db.open("3");
    doc2.updated = "totally";
    db.save(doc2);
    all_seq = db.allDocsBySeq();
    
    // the update should make doc id 3 have the last seq num
    T(all_seq.rows.length == 4);
    T(all_seq.rows[3].id == "3");

    // ok now lets see what happens with include docs
    all_seq = db.allDocsBySeq({include_docs: true});
    T(all_seq.rows.length == 4);
    T(all_seq.rows[3].id == "3");
    T(all_seq.rows[3].doc.updated == "totally");

    // and on the deleted one, no doc
    T(all_seq.rows[2].value.deleted);
    T(!all_seq.rows[2].doc);
    
    // test the all docs collates sanely
    db.save({_id: "Z", foo: "Z"});
    db.save({_id: "a", foo: "a"});

    var rows = db.allDocs({startkey: "Z", endkey: "Z"}).rows;
    T(rows.length == 1);
  },
  
  // Do some edit conflict detection tests
  conflicts: function(debug) {
    var db = new CouchDB("test_suite_db");
    db.deleteDb();
    db.createDb();
    if (debug) debugger;

    // create a doc and save
    var doc = {_id:"foo",a:1,b:1};
    T(db.save(doc).ok);

    // reopen
    var doc2 = db.open(doc._id);

    // ensure the revisions are the same
    T(doc._id == doc2._id && doc._rev == doc2._rev);

    // edit the documents.
    doc.a = 2;
    doc2.a = 3;

    // save one document
    T(db.save(doc).ok);

    // save the other document
    try {
      db.save(doc2);  // this should generate a conflict exception
      T("no save conflict 1" && false); // we shouldn't hit here
    } catch (e) {
      T(e.error == "conflict");
    }

    // Now clear out the _rev member and save. This indicates this document is
    // new, not based on an existing revision.
    doc2._rev = undefined;
    try {
      db.save(doc2); // this should generate a conflict exception
      T("no save conflict 2" && false); // we shouldn't hit here
    } catch (e) {
      T(e.error == "conflict");
    }

    // Now delete the document from the database
    T(db.deleteDoc(doc).ok);

    T(db.save(doc2).ok);  // we can save a new document over a deletion without
                          // knowing the deletion rev.
  },

  recreate_doc: function(debug) {
    var db = new CouchDB("test_suite_db");
    db.deleteDb();
    db.createDb();
    if (debug) debugger;

    // First create a new document with the ID "foo", and delete it again
    var doc = {_id: "foo", a: "bar", b: 42};
    T(db.save(doc).ok);
    T(db.deleteDoc(doc).ok);

    // Now create a new document with the same ID, save it, and then modify it
    // This should work fine, but currently results in a conflict error, at
    // least "sometimes"
    for (var i = 0; i < 10; i++) {
      doc = {_id: "foo"};
      T(db.save(doc).ok);
      doc = db.open("foo");
      doc.a = "baz";
      try {
        T(db.save(doc).ok);
      } finally {
        // And now, we can't even delete the document anymore :/
        T(db.deleteDoc(doc).rev != undefined);
      }
    }
  },

  copy_move_doc: function(debug) {
    var db = new CouchDB("test_suite_db");
    db.deleteDb();
    db.createDb();
    if (debug) debugger;

    // copy a doc
    T(db.save({_id:"doc_to_be_copied",v:1}).ok);
    var xhr = CouchDB.request("COPY", "/test_suite_db/doc_to_be_copied", {
      headers: {"Destination":"doc_that_was_copied"}
    });

    T(xhr.status == 201);
    T(db.open("doc_that_was_copied").v == 1);

    // move a doc

    // test error condition
    var xhr = CouchDB.request("MOVE", "/test_suite_db/doc_to_be_copied", {
      headers: {"Destination":"doc_that_was_moved"}
    });
    T(xhr.status == 400); // bad request, MOVE requires source rev.

    var rev = db.open("doc_to_be_copied")._rev;
    var xhr = CouchDB.request("MOVE", "/test_suite_db/doc_to_be_copied?rev=" + rev, {
      headers: {"Destination":"doc_that_was_moved"}
    });

    T(xhr.status == 201);
    T(db.open("doc_that_was_moved").v == 1);
    T(db.open("doc_to_be_copied") == null);

    // COPY with existing target
    T(db.save({_id:"doc_to_be_copied",v:1}).ok);
    var doc = db.save({_id:"doc_to_be_overwritten",v:2});
    T(doc.ok);

    // error condition
    var xhr = CouchDB.request("COPY", "/test_suite_db/doc_to_be_copied", {
        headers: {"Destination":"doc_to_be_overwritten"}
    });
    T(xhr.status == 412); // conflict

    var rev = db.open("doc_to_be_overwritten")._rev;
    var xhr = CouchDB.request("COPY", "/test_suite_db/doc_to_be_copied", {
      headers: {"Destination":"doc_to_be_overwritten?rev=" + rev}
    });
    T(xhr.status == 201);

    var over = db.open("doc_to_be_overwritten");
    T(rev != over._rev);
    T(over.v == 1);
  },

  uuids: function(debug) {
    var db = new CouchDB("test_suite_db");
    db.deleteDb();
    db.createDb();
    if (debug) debugger;
    
    // a single UUID without an explicit count
    var xhr = CouchDB.request("POST", "/_uuids");
    T(xhr.status == 200);
    var result = JSON.parse(xhr.responseText);
    T(result.uuids.length == 1);
    var first = result.uuids[0];

    // a single UUID with an explicit count
    xhr = CouchDB.request("POST", "/_uuids?count=1");
    T(xhr.status == 200);
    result = JSON.parse(xhr.responseText);
    T(result.uuids.length == 1);
    var second = result.uuids[0];
    T(first != second);

    // no collisions with 1,000 UUIDs
    xhr = CouchDB.request("POST", "/_uuids?count=1000");
    T(xhr.status == 200);
    result = JSON.parse(xhr.responseText);
    T( result.uuids.length == 1000 );
    var seen = {};
    for(var i in result.uuids) {
      var id = result.uuids[i];
      T(seen[id] === undefined);
      seen[id] = 1;
    }
    
    // check our library
  },
  
  bulk_docs: function(debug) {
    var db = new CouchDB("test_suite_db");
    db.deleteDb();
    db.createDb();
    if (debug) debugger;

    var docs = makeDocs(5);

    // Create the docs
    var result = db.bulkSave(docs);
    T(result.ok);
    T(result.new_revs.length == 5);
    for (var i = 0; i < 5; i++) {
      T(result.new_revs[i].id == docs[i]._id);
      T(result.new_revs[i].rev);
      docs[i].string = docs[i].string + ".00";
    }

    // Update the docs
    result = db.bulkSave(docs);
    T(result.ok);
    T(result.new_revs.length == 5);
    for (i = 0; i < 5; i++) {
      T(result.new_revs[i].id == i.toString());
      docs[i]._deleted = true;
    }

    // Delete the docs
    result = db.bulkSave(docs);
    T(result.ok);
    T(result.new_revs.length == 5);
    for (i = 0; i < 5; i++) {
      T(db.open(docs[i]._id) == null);
    }
    
    // verify creating a document with no id returns a new id
    var req = CouchDB.request("POST", "/test_suite_db/_bulk_docs", {
      body: JSON.stringify({"docs": [{"foo":"bar"}]})
    });
    result = JSON.parse(req.responseText);
    
    T(result.new_revs[0].id != "");
    T(result.new_revs[0].rev != "");
  },

  // test saving a semi-large quanitity of documents and do some view queries.
  lots_of_docs: function(debug) {
    var db = new CouchDB("test_suite_db");
    db.deleteDb();
    db.createDb();
    if (debug) debugger;

    // keep number lowish for now to keep tests fasts. Crank up manually to
    // to really test.
    var numDocsToCreate = 500;

    for(var i=0; i < numDocsToCreate; i += 100) {
        var createNow = Math.min(numDocsToCreate - i, 100);
        var docs = makeDocs(i, i + createNow);
        T(db.bulkSave(docs).ok);
    }

    // query all documents, and return the doc.integer member as a key.
    results = db.query(function(doc){ emit(doc.integer, null) });

    T(results.total_rows == numDocsToCreate);

    // validate the keys are ordered ascending
    for(var i=0; i<numDocsToCreate; i++) {
      T(results.rows[i].key==i);
    }

    // do the query again, but with descending output
    results = db.query(function(doc){ emit(doc.integer, null) }, null, {
      descending: true
    });

    T(results.total_rows == numDocsToCreate);

    // validate the keys are ordered descending
    for(var i=0; i<numDocsToCreate; i++) {
      T(results.rows[numDocsToCreate-1-i].key==i);
    }

    // Check _all_docs with descending=true again (now that there are many docs)
    // this fails, see COUCHDB-109
    // var desc = db.allDocs({descending:true});
    // T(desc.total_rows == desc.rows.length);
  },

  reduce: function(debug) {
    var db = new CouchDB("test_suite_db");
    db.deleteDb();
    db.createDb();
    if (debug) debugger;
    var numDocs = 500
    var docs = makeDocs(1,numDocs + 1);
    T(db.bulkSave(docs).ok);
    var summate = function(N) {return (N+1)*N/2;};

    var map = function (doc) {
        emit(doc.integer, doc.integer);
        emit(doc.integer, doc.integer)};
    var reduce = function (keys, values) { return sum(values); };
    var result = db.query(map, reduce);
    T(result.rows[0].value == 2*summate(numDocs));

    result = db.query(map, reduce, {startkey: 4, endkey: 4});
    T(result.rows[0].value == 8);

    result = db.query(map, reduce, {startkey: 4, endkey: 5});
    T(result.rows[0].value == 18);

    result = db.query(map, reduce, {startkey: 4, endkey: 6});
    T(result.rows[0].value == 30);

    result = db.query(map, reduce, {group:true, limit:3});
    T(result.rows[0].value == 2);
    T(result.rows[1].value == 4);
    T(result.rows[2].value == 6);

    for(var i=1; i<numDocs/2; i+=30) {
      result = db.query(map, reduce, {startkey: i, endkey: numDocs - i});
      T(result.rows[0].value == 2*(summate(numDocs-i) - summate(i-1)));
    }

    db.deleteDb();
    db.createDb();

    for(var i=1; i <= 5; i++) {

      for(var j=0; j < 10; j++) {
        // these docs are in the order of the keys collation, for clarity
        var docs = [];
        docs.push({keys:["a"]});
        docs.push({keys:["a"]});
        docs.push({keys:["a", "b"]});
        docs.push({keys:["a", "b"]});
        docs.push({keys:["a", "b", "c"]});
        docs.push({keys:["a", "b", "d"]});
        docs.push({keys:["a", "c", "d"]});
        docs.push({keys:["d"]});
        docs.push({keys:["d", "a"]});
        docs.push({keys:["d", "b"]});
        docs.push({keys:["d", "c"]});
        T(db.bulkSave(docs).ok);
        T(db.info().doc_count == ((i - 1) * 10 * 11) + ((j + 1) * 11));
      }

      map = function (doc) {emit(doc.keys, 1)};
      reduce = function (keys, values) { return sum(values); };

      var results = db.query(map, reduce, {group:true});

      //group by exact key match
      T(equals(results.rows[0], {key:["a"],value:20*i}));
      T(equals(results.rows[1], {key:["a","b"],value:20*i}));
      T(equals(results.rows[2], {key:["a", "b", "c"],value:10*i}));
      T(equals(results.rows[3], {key:["a", "b", "d"],value:10*i}));

      // test to make sure group reduce and limit params provide valid json
      var results = db.query(map, reduce, {group: true, limit: 2});
      T(equals(results.rows[0], {key: ["a"], value: 20*i}));
      T(equals(results.rows.length, 2));

      //group by the first element in the key array
      var results = db.query(map, reduce, {group_level:1});
      T(equals(results.rows[0], {key:["a"],value:70*i}));
      T(equals(results.rows[1], {key:["d"],value:40*i}));

      //group by the first 2 elements in the key array
      var results = db.query(map, reduce, {group_level:2});
      T(equals(results.rows[0], {key:["a"],value:20*i}));
      T(equals(results.rows[1], {key:["a","b"],value:40*i}));
      T(equals(results.rows[2], {key:["a","c"],value:10*i}));
      T(equals(results.rows[3], {key:["d"],value:10*i}));
      T(equals(results.rows[4], {key:["d","a"],value:10*i}));
      T(equals(results.rows[5], {key:["d","b"],value:10*i}));
      T(equals(results.rows[6], {key:["d","c"],value:10*i}));
    }

    // now test out more complex reductions that need to use the combine option.

    db.deleteDb();
    db.createDb();


    var map = function (doc) {emit(doc.val, doc.val)};
    var reduceCombine = function (keys, values, rereduce) {
        // This computes the standard deviation of the mapped results
        var stdDeviation=0.0;
        var count=0;
        var total=0.0;
        var sqrTotal=0.0;

        if (!rereduce) {
          // This is the reduce phase, we are reducing over emitted values from
          // the map functions.
          for(var i in values) {
            total = total + values[i];
            sqrTotal = sqrTotal + (values[i] * values[i]);
          }
          count = values.length;
        }
        else {
          // This is the rereduce phase, we are re-reducing previosuly
          // reduced values.
          for(var i in values) {
            count = count + values[i].count;
            total = total + values[i].total;
            sqrTotal = sqrTotal + values[i].sqrTotal;
          }
        }

        var variance =  (sqrTotal - ((total * total)/count)) / count;
        stdDeviation = Math.sqrt(variance);

        // the reduce result. It contains enough information to be rereduced
        // with other reduce results.
        return {"stdDeviation":stdDeviation,"count":count,
            "total":total,"sqrTotal":sqrTotal};
      };

      // Save a bunch a docs.

    for(var i=0; i < 10; i++) {
      var docs = [];
      docs.push({val:10});
      docs.push({val:20});
      docs.push({val:30});
      docs.push({val:40});
      docs.push({val:50});
      docs.push({val:60});
      docs.push({val:70});
      docs.push({val:80});
      docs.push({val:90});
      docs.push({val:100});
      T(db.bulkSave(docs).ok);
    }
    
    var results = db.query(map, reduceCombine);
    
    var difference = results.rows[0].value.stdDeviation - 28.722813232690143;
    // account for floating point rounding error
    T(Math.abs(difference) < 0.0000000001);
    
  },

  reduce_false: function(debug) {
    var db = new CouchDB("test_suite_db");
    db.deleteDb();
    db.createDb();
    if (debug) debugger;

    var numDocs = 5;
    var docs = makeDocs(1,numDocs + 1);
    T(db.bulkSave(docs).ok);
    var summate = function(N) {return (N+1)*N/2;};

    var designDoc = {
      _id:"_design/test",
      language: "javascript",
      views: {
        summate: {map:"function (doc) {emit(doc.integer, doc.integer)};",
                  reduce:"function (keys, values) { return sum(values); };"},
      }
    };
    T(db.save(designDoc).ok);

    // Test that the reduce works
    var res = db.view('test/summate');
    T(res.rows.length == 1 && res.rows[0].value == summate(5));
    
    //Test that we get our docs back
    res = db.view('test/summate', {reduce: false});
    T(res.rows.length == 5);
    for(var i=0; i<5; i++)
    {
      T(res.rows[i].value == i+1);
    }
  },

  multiple_rows: function(debug) {
    var db = new CouchDB("test_suite_db");
    db.deleteDb();
    db.createDb();
    if (debug) debugger;

    var nc = {_id:"NC", cities:["Charlotte", "Raleigh"]};
    var ma = {_id:"MA", cities:["Boston", "Lowell", "Worcester", "Cambridge", "Springfield"]};
    var fl = {_id:"FL", cities:["Miami", "Tampa", "Orlando", "Springfield"]};

    T(db.save(nc).ok);
    T(db.save(ma).ok);
    T(db.save(fl).ok);

    var generateListOfCitiesAndState = "function(doc) {" +
    " for (var i = 0; i < doc.cities.length; i++)" +
    "  emit(doc.cities[i] + \", \" + doc._id, null);" +
    "}";

    var results = db.query(generateListOfCitiesAndState);
    var rows = results.rows;

    T(rows[0].key == "Boston, MA");
    T(rows[1].key == "Cambridge, MA");
    T(rows[2].key == "Charlotte, NC");
    T(rows[3].key == "Lowell, MA");
    T(rows[4].key == "Miami, FL");
    T(rows[5].key == "Orlando, FL");
    T(rows[6].key == "Raleigh, NC");
    T(rows[7].key == "Springfield, FL");
    T(rows[8].key == "Springfield, MA");
    T(rows[9].key == "Tampa, FL");
    T(rows[10].key == "Worcester, MA");

    // add another city to NC
    nc.cities.push("Wilmington");
    T(db.save(nc).ok);

    var results = db.query(generateListOfCitiesAndState);
    var rows = results.rows;

    T(rows[0].key == "Boston, MA");
    T(rows[1].key == "Cambridge, MA");
    T(rows[2].key == "Charlotte, NC");
    T(rows[3].key == "Lowell, MA");
    T(rows[4].key == "Miami, FL");
    T(rows[5].key == "Orlando, FL");
    T(rows[6].key == "Raleigh, NC");
    T(rows[7].key == "Springfield, FL");
    T(rows[8].key == "Springfield, MA");
    T(rows[9].key == "Tampa, FL");
    T(rows[10].key == "Wilmington, NC");
    T(rows[11].key == "Worcester, MA");

    // now delete MA
    T(db.deleteDoc(ma).ok);

    var results = db.query(generateListOfCitiesAndState);
    var rows = results.rows;

    T(rows[0].key == "Charlotte, NC");
    T(rows[1].key == "Miami, FL");
    T(rows[2].key == "Orlando, FL");
    T(rows[3].key == "Raleigh, NC");
    T(rows[4].key == "Springfield, FL");
    T(rows[5].key == "Tampa, FL");
    T(rows[6].key == "Wilmington, NC");
  },

  large_docs: function(debug) {
    var db = new CouchDB("test_suite_db");
    db.deleteDb();
    db.createDb();
    if (debug) debugger;

    var longtext = "0123456789\n";

    for (var i=0; i<10; i++) {
      longtext = longtext + longtext
    }
    T(db.save({"longtest":longtext}).ok);
    T(db.save({"longtest":longtext}).ok);
    T(db.save({"longtest":longtext}).ok);
    T(db.save({"longtest":longtext}).ok);

    // query all documents, and return the doc.foo member as a key.
    results = db.query(function(doc){
        emit(null, doc.longtest);
    });
  },

  utf8: function(debug) {
    var db = new CouchDB("test_suite_db");
    db.deleteDb();
    db.createDb();
    if (debug) debugger;

    var texts = [];

    texts[0] = "1. Ascii: hello"
    texts[1] = "2. Russian: На берегу пустынных волн"
    texts[2] = "3. Math: ∮ E⋅da = Q,  n → ∞, ∑ f(i) = ∏ g(i),"
    texts[3] = "4. Geek: STARGΛ̊TE SG-1"
    texts[4] = "5. Braille: ⡌⠁⠧⠑ ⠼⠁⠒  ⡍⠜⠇⠑⠹⠰⠎ ⡣⠕⠌"

    // check that we can save a reload with full fidelity
    for (var i=0; i<texts.length; i++) {
      T(db.save({_id:i.toString(), text:texts[i]}).ok);
    }

    for (var i=0; i<texts.length; i++) {
      T(db.open(i.toString()).text == texts[i]);
    }

    // check that views and key collation don't blow up
    var rows = db.query(function(doc) { emit(null, doc.text) }).rows;
    for (var i=0; i<texts.length; i++) {
      T(rows[i].value == texts[i]);
    }
  },

  attachments: function(debug) {
    var db = new CouchDB("test_suite_db");
    db.deleteDb();
    db.createDb();
    if (debug) debugger;

    var binAttDoc = {
      _id: "bin_doc",
      _attachments:{
        "foo.txt": {
          content_type:"text/plain",
          data: "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ="
        }
      }
    }

    var save_response = db.save(binAttDoc);
    T(save_response.ok);

    var xhr = CouchDB.request("GET", "/test_suite_db/bin_doc/foo.txt");
    T(xhr.responseText == "This is a base64 encoded text");
    T(xhr.getResponseHeader("Content-Type") == "text/plain");
    T(xhr.getResponseHeader("Etag") == save_response.rev);

    // empty attachment
    var binAttDoc2 = {
      _id: "bin_doc2",
      _attachments:{
        "foo.txt": {
          content_type:"text/plain",
          data: ""
        }
      }
    }

    T(db.save(binAttDoc2).ok);

    var xhr = CouchDB.request("GET", "/test_suite_db/bin_doc2/foo.txt");
    T(xhr.responseText.length == 0);
    T(xhr.getResponseHeader("Content-Type") == "text/plain");

    // test RESTful doc API

    var xhr = CouchDB.request("PUT", "/test_suite_db/bin_doc2/foo2.txt?rev=" + binAttDoc2._rev, {
      body:"This is no base64 encoded text",
      headers:{"Content-Type": "text/plain;charset=utf-8"}
    });
    T(xhr.status == 201);
    var rev = JSON.parse(xhr.responseText).rev;

    binAttDoc2 = db.open("bin_doc2");

    T(binAttDoc2._attachments["foo.txt"] !== undefined);
    T(binAttDoc2._attachments["foo2.txt"] !== undefined);
    T(binAttDoc2._attachments["foo2.txt"].content_type == "text/plain;charset=utf-8");
    T(binAttDoc2._attachments["foo2.txt"].length == 30);

    var xhr = CouchDB.request("GET", "/test_suite_db/bin_doc2/foo2.txt");
    T(xhr.responseText == "This is no base64 encoded text");
    T(xhr.getResponseHeader("Content-Type") == "text/plain;charset=utf-8");
    
    // test without rev, should fail
    var xhr = CouchDB.request("DELETE", "/test_suite_db/bin_doc2/foo2.txt");
    T(xhr.status == 412);

    // test with rev, should not fail
    var xhr = CouchDB.request("DELETE", "/test_suite_db/bin_doc2/foo2.txt?rev=" + rev);
    T(xhr.status == 200);
    
    
    // test binary data
    var bin_data = "JHAPDO*AU£PN ){(3u[d 93DQ9¡€])}    ææøo'∂ƒæ≤çæππ•¥∫¶®#†π¶®¥π€ª®˙π8np";
    var xhr = CouchDB.request("PUT", "/test_suite_db/bin_doc3/attachment.txt", {
      headers:{"Content-Type":"text/plain;charset=utf-8"},
      body:bin_data
    });
    T(xhr.status == 201);
    var rev = JSON.parse(xhr.responseText).rev;
    
    var xhr = CouchDB.request("GET", "/test_suite_db/bin_doc3/attachment.txt");
    T(xhr.responseText == bin_data);
    T(xhr.getResponseHeader("Content-Type") == "text/plain;charset=utf-8");
    
    var xhr = CouchDB.request("PUT", "/test_suite_db/bin_doc3/attachment.txt", {
      headers:{"Content-Type":"text/plain;charset=utf-8"},
      body:bin_data
    });
    T(xhr.status == 412);

    var xhr = CouchDB.request("PUT", "/test_suite_db/bin_doc3/attachment.txt?rev=" + rev, {
      headers:{"Content-Type":"text/plain;charset=utf-8"},
      body:bin_data
    });
    T(xhr.status == 201);
    var rev = JSON.parse(xhr.responseText).rev;

    var xhr = CouchDB.request("GET", "/test_suite_db/bin_doc3/attachment.txt");
    T(xhr.responseText == bin_data);
    T(xhr.getResponseHeader("Content-Type") == "text/plain;charset=utf-8");

    var xhr = CouchDB.request("GET", "/test_suite_db/bin_doc3/attachment.txt?rev=" + rev);
    T(xhr.responseText == bin_data);
    T(xhr.getResponseHeader("Content-Type") == "text/plain;charset=utf-8");

    var xhr = CouchDB.request("DELETE", "/test_suite_db/bin_doc3/attachment.txt?rev=" + rev);
    T(xhr.status == 200);
    
    var xhr = CouchDB.request("GET", "/test_suite_db/bin_doc3/attachment.txt?rev=" + rev);
    T(xhr.status == 404);

    // empty attachments
    var xhr = CouchDB.request("PUT", "/test_suite_db/bin_doc4/attachment.txt", {
      headers:{"Content-Type":"text/plain;charset=utf-8"},
      body:""
    });
    T(xhr.status == 201);
    var rev = JSON.parse(xhr.responseText).rev;

    var xhr = CouchDB.request("GET", "/test_suite_db/bin_doc4/attachment.txt");
    T(xhr.status == 200);
    T(xhr.responseText.length == 0);
    
    // overwrite previsously empty attachment
    var xhr = CouchDB.request("PUT", "/test_suite_db/bin_doc4/attachment.txt?rev=" + rev, {
      headers:{"Content-Type":"text/plain;charset=utf-8"},
      body:"This is a string"
    });
    T(xhr.status == 201);

    var xhr = CouchDB.request("GET", "/test_suite_db/bin_doc4/attachment.txt");
    T(xhr.status == 200);
    T(xhr.responseText == "This is a string");

  },

  attachment_paths : function(debug) {
    if (debug) debugger;
    var dbNames = ["test_suite_db", "test_suite_db/with_slashes"];
    for (var i=0; i < dbNames.length; i++) {
      var db = new CouchDB(dbNames[i]);
      var dbName = encodeURIComponent(dbNames[i]);
      db.deleteDb();
      db.createDb();

      // first just save a regular doc with an attachment that has a slash in the url.
      // (also gonna run an encoding check case)
      var binAttDoc = {
        _id: "bin_doc",
        _attachments:{
          "foo/bar.txt": {
            content_type:"text/plain",
            data: "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ="
          },
          "foo%2Fbaz.txt": {
            content_type:"text/plain",
            data: "V2UgbGlrZSBwZXJjZW50IHR3byBGLg=="
          }
        }
      }

      T(db.save(binAttDoc).ok);

      var xhr = CouchDB.request("GET", "/"+dbName+"/bin_doc/foo/bar.txt");
      T(xhr.responseText == "This is a base64 encoded text");
      T(xhr.getResponseHeader("Content-Type") == "text/plain");

      // lets try it with an escaped attachment id...
      // weird that it's at two urls
      var xhr = CouchDB.request("GET", "/"+dbName+"/bin_doc/foo%2Fbar.txt");
      T(xhr.status == 200); 
      // xhr.responseText == "This is a base64 encoded text"

      var xhr = CouchDB.request("GET", "/"+dbName+"/bin_doc/foo/baz.txt");
      T(xhr.status == 404);

      var xhr = CouchDB.request("GET", "/"+dbName+"/bin_doc/foo%252Fbaz.txt");
      T(xhr.status == 200); 
      T(xhr.responseText == "We like percent two F.");

      // require a _rev to PUT
      var xhr = CouchDB.request("PUT", "/"+dbName+"/bin_doc/foo/attachment.txt", {
        headers:{"Content-Type":"text/plain;charset=utf-8"},
        body:"Just some text"
      });
      T(xhr.status == 412);    

      var xhr = CouchDB.request("PUT", "/"+dbName+"/bin_doc/foo/bar2.txt?rev=" + binAttDoc._rev, {
        body:"This is no base64 encoded text",
        headers:{"Content-Type": "text/plain;charset=utf-8"}
      });
      T(xhr.status == 201);
      var rev = JSON.parse(xhr.responseText).rev;

      binAttDoc = db.open("bin_doc");

      T(binAttDoc._attachments["foo/bar.txt"] !== undefined);
      T(binAttDoc._attachments["foo%2Fbaz.txt"] !== undefined);
      T(binAttDoc._attachments["foo/bar2.txt"] !== undefined);
      T(binAttDoc._attachments["foo/bar2.txt"].content_type == "text/plain;charset=utf-8");
      T(binAttDoc._attachments["foo/bar2.txt"].length == 30);

      //// now repeat the while thing with a design doc
    
      // first just save a regular doc with an attachment that has a slash in the url.
      // (also gonna run an encoding check case)
      var binAttDoc = {
        _id: "_design/bin_doc",
        _attachments:{
          "foo/bar.txt": {
            content_type:"text/plain",
            data: "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ="
          },
          "foo%2Fbaz.txt": {
            content_type:"text/plain",
            data: "V2UgbGlrZSBwZXJjZW50IHR3byBGLg=="
          }
        }
      }

      T(db.save(binAttDoc).ok);

      var xhr = CouchDB.request("GET", "/"+dbName+"/_design%2Fbin_doc/foo/bar.txt");
      T(xhr.responseText == "This is a base64 encoded text");
      T(xhr.getResponseHeader("Content-Type") == "text/plain");

      // lets try it with an escaped attachment id...
      // weird that it's at two urls
      var xhr = CouchDB.request("GET", "/"+dbName+"/_design%2Fbin_doc/foo%2Fbar.txt");
      T(xhr.responseText == "This is a base64 encoded text");
      T(xhr.status == 200);

      // err, 3 urls
      var xhr = CouchDB.request("GET", "/"+dbName+"/_design/bin_doc/foo%2Fbar.txt");
      T(xhr.responseText == "This is a base64 encoded text");
      T(xhr.status == 200);

      // I mean um, 4 urls
      var xhr = CouchDB.request("GET", "/"+dbName+"/_design/bin_doc/foo/bar.txt");
      T(xhr.responseText == "This is a base64 encoded text");
      T(xhr.status == 200);

      var xhr = CouchDB.request("GET", "/"+dbName+"/_design%2Fbin_doc/foo/baz.txt");
      T(xhr.status == 404);

      var xhr = CouchDB.request("GET", "/"+dbName+"/_design%2Fbin_doc/foo%252Fbaz.txt");
      T(xhr.status == 200); 
      T(xhr.responseText == "We like percent two F.");

      // require a _rev to PUT
      var xhr = CouchDB.request("PUT", "/"+dbName+"/_design%2Fbin_doc/foo/attachment.txt", {
        headers:{"Content-Type":"text/plain;charset=utf-8"},
        body:"Just some text"
      });
      T(xhr.status == 412);    

      var xhr = CouchDB.request("PUT", "/"+dbName+"/_design%2Fbin_doc/foo/bar2.txt?rev=" + binAttDoc._rev, {
        body:"This is no base64 encoded text",
        headers:{"Content-Type": "text/plain;charset=utf-8"}
      });
      T(xhr.status == 201);
      var rev = JSON.parse(xhr.responseText).rev;

      binAttDoc = db.open("_design/bin_doc");

      T(binAttDoc._attachments["foo/bar.txt"] !== undefined);
      T(binAttDoc._attachments["foo/bar2.txt"] !== undefined);
      T(binAttDoc._attachments["foo/bar2.txt"].content_type == "text/plain;charset=utf-8");
      T(binAttDoc._attachments["foo/bar2.txt"].length == 30);
    }
  },

  attachment_views: function(debug) {

    var db = new CouchDB("test_suite_db");
    db.deleteDb();
    db.createDb();
    if (debug) debugger;

    // count attachments in a view

    db.bulkSave(makeDocs(0, 10));

    db.bulkSave(makeDocs(10, 20, {
      _attachments:{
        "foo.txt": {
          content_type:"text/plain",
          data: "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ="
        }
      }
    }));

    db.bulkSave(makeDocs(20, 30, {
      _attachments:{
        "foo.txt": {
          content_type:"text/plain",
          data: "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ="
        },
        "bar.txt": {
          content_type:"text/plain",
          data: "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ="
        }
      }
    }));

    db.bulkSave(makeDocs(30, 40, {
      _attachments:{
        "foo.txt": {
          content_type:"text/plain",
          data: "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ="
        },
        "bar.txt": {
          content_type:"text/plain",
          data: "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ="
        },
        "baz.txt": {
          content_type:"text/plain",
          data: "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ="
        }
      }
    }));

    var mapFunction = function(doc) {
      var count = 0;

      for(var idx in doc._attachments) {
        count = count + 1;
      }

      emit(parseInt(doc._id), count);
    }

    var reduceFunction = function(key, values) {
      return sum(values);
    }
    
    var result = db.query(mapFunction, reduceFunction);

    T(result.rows.length == 1);
    T(result.rows[0].value == 60);

    var result = db.query(mapFunction, reduceFunction, {
      startkey:10,
      endkey:19
    });

    T(result.rows.length == 1);
    T(result.rows[0].value == 10);

    var result = db.query(mapFunction, reduceFunction, {
      startkey:20,
      endkey:29
    });

    T(result.rows.length == 1);
    T(result.rows[0].value == 20);

  },

  design_paths : function(debug) {
    if (debug) debugger;
    var dbNames = ["test_suite_db", "test_suite_db/with_slashes"];
    for (var i=0; i < dbNames.length; i++) {
      var db = new CouchDB(dbNames[i]);
      var dbName = encodeURIComponent(dbNames[i]);
      db.deleteDb();
      db.createDb();
      
      // create a ddoc w bulk_docs
      db.bulkSave([{
        _id : "_design/test",
        views : {
          "testing" : {
            "map" : "function(){emit(1,1)}"
          }
        }
      }]);

      // ddoc is getable
      var xhr = CouchDB.request("GET", "/"+dbName+"/_design/test");
      var resp = JSON.parse(xhr.responseText);
      T(resp._id == "_design/test");

      // it's at 2 urls...
      var xhr = CouchDB.request("GET", "/"+dbName+"/_design%2Ftest");
      var resp = JSON.parse(xhr.responseText);
      T(resp._id == "_design/test");

      // ensure that views are addressable
      resp = db.view("test/testing")
      T(resp.total_rows == 0)

      // create a ddoc by putting to url with raw slash
      var xhr = CouchDB.request("PUT", "/"+dbName+"/_design/test2",{
        body : JSON.stringify({
          _id : "_design/test2",
          views : {
            "testing" : {
              "map" : "function(){emit(1,1)}"
            }
          }
        })
      });

      // ddoc is getable
      var xhr = CouchDB.request("GET", "/"+dbName+"/_design/test2");
      var resp = JSON.parse(xhr.responseText);
      T(resp._id == "_design/test2");

      // it's at 2 urls...
      var xhr = CouchDB.request("GET", "/"+dbName+"/_design%2Ftest2");
      var resp = JSON.parse(xhr.responseText);
      T(resp._id == "_design/test2");

      // ensure that views are addressable
      resp = db.view("test2/testing");
      T(resp.total_rows == 0);
    };
  },

  content_negotiation: function(debug) {
    var db = new CouchDB("test_suite_db");
    db.deleteDb();
    db.createDb();
    if (debug) debugger;
    var xhr;

    xhr = CouchDB.request("GET", "/test_suite_db/");
    T(xhr.getResponseHeader("Content-Type") == "text/plain;charset=utf-8");

    xhr = CouchDB.request("GET", "/test_suite_db/", {
      headers: {"Accept": "text/html;text/plain;*/*"}
    });
    T(xhr.getResponseHeader("Content-Type") == "text/plain;charset=utf-8");

    xhr = CouchDB.request("GET", "/test_suite_db/", {
      headers: {"Accept": "application/json"}
    });
    T(xhr.getResponseHeader("Content-Type") == "application/json");
  },

  design_docs: function(debug) {
    var db = new CouchDB("test_suite_db");
    db.deleteDb();
    db.createDb();
    if (debug) debugger;

    var numDocs = 500;

    function makebigstring(power) {
      var str = "a";
      while(power-- > 0) {
        str = str + str;
      }
      return str;
    }

    var designDoc = {
      _id:"_design/test", // turn off couch.js id escaping?
      language: "javascript",
      views: {
        all_docs_twice: {map: "function(doc) { emit(doc.integer, null); emit(doc.integer, null) }"},
        no_docs: {map: "function(doc) {}"},
        single_doc: {map: "function(doc) { if (doc._id == \"1\") { emit(1, null) }}"},
        summate: {map:"function (doc) {emit(doc.integer, doc.integer)};",
                  reduce:"function (keys, values) { return sum(values); };"},
        summate2: {map:"function (doc) {emit(doc.integer, doc.integer)};",
                  reduce:"function (keys, values) { return sum(values); };"},
        huge_src_and_results: {map: "function(doc) { if (doc._id == \"1\") { emit(\"" + makebigstring(16) + "\", null) }}",
                  reduce:"function (keys, values) { return \"" + makebigstring(16) + "\"; };"}
      }
    }
    T(db.save(designDoc).ok);

    T(db.bulkSave(makeDocs(1, numDocs + 1)).ok);

    // test that the _all_docs view returns correctly with keys
    var results = db.allDocs({startkey:"_design", endkey:"_design0"});
    T(results.rows.length == 1);

    for (var loop = 0; loop < 2; loop++) {
      var rows = db.view("test/all_docs_twice").rows;
      for (var i = 0; i < numDocs; i++) {
        T(rows[2*i].key == i+1);
        T(rows[(2*i)+1].key == i+1);
      }
      T(db.view("test/no_docs").total_rows == 0)
      T(db.view("test/single_doc").total_rows == 1)
      T(db.ensureFullCommit().ok);
      restartServer();
    };
    
    // test when language not specified, Javascript is implied
    var designDoc2 = {
      _id:"_design/test2",
      // language: "javascript", 
      views: {
        single_doc: {map: "function(doc) { if (doc._id == \"1\") { emit(1, null) }}"}
      }
    };
    
    T(db.save(designDoc2).ok);
    T(db.view("test2/single_doc").total_rows == 1);

    var summate = function(N) {return (N+1)*N/2;};
    var result = db.view("test/summate");
    T(result.rows[0].value == summate(numDocs));

    result = db.view("test/summate", {startkey:4,endkey:4});
    T(result.rows[0].value == 4);

    result = db.view("test/summate", {startkey:4,endkey:5});
    T(result.rows[0].value == 9);

    result = db.view("test/summate", {startkey:4,endkey:6});
    T(result.rows[0].value == 15);

    // Verify that a shared index (view def is an exact copy of "summate")
    // does not confuse the reduce stage
    result = db.view("test/summate2", {startkey:4,endkey:6});
    T(result.rows[0].value == 15);

    for(var i=1; i<numDocs/2; i+=30) {
      result = db.view("test/summate", {startkey:i,endkey:numDocs-i});
      T(result.rows[0].value == summate(numDocs-i) - summate(i-1));
    }

    T(db.deleteDoc(designDoc).ok);
    T(db.open(designDoc._id) == null);
    T(db.view("test/no_docs") == null);
    
    T(db.ensureFullCommit().ok);
    restartServer();
    T(db.open(designDoc._id) == null);
    T(db.view("test/no_docs") == null);
  },

  view_collation: function(debug) {
    var db = new CouchDB("test_suite_db");
    db.deleteDb();
    db.createDb();
    if (debug) debugger;

    // NOTE, the values are already in their correct sort order. Consider this
    // a specification of collation of json types.

    var values = []

    // special values sort before all other types
    values.push(null)
    values.push(false)
    values.push(true)

    // then numbers
    values.push(1)
    values.push(2)
    values.push(3.0)
    values.push(4)

    // then text, case sensitive
    values.push("a")
    values.push("A")
    values.push("aa")
    values.push("b")
    values.push("B")
    values.push("ba")
    values.push("bb")

    // then arrays. compared element by element until different.
    // Longer arrays sort after their prefixes
    values.push(["a"])
    values.push(["b"])
    values.push(["b","c"])
    values.push(["b","c", "a"])
    values.push(["b","d"])
    values.push(["b","d", "e"])

    // then object, compares each key value in the list until different.
    // larger objects sort after their subset objects.
    values.push({a:1})
    values.push({a:2})
    values.push({b:1})
    values.push({b:2})
    values.push({b:2, a:1}) // Member order does matter for collation.
                            // CouchDB preserves member order
                            // but doesn't require that clients will.
                            // (this test might fail if used with a js engine
                            // that doesn't preserve order)
    values.push({b:2, c:2})

    for (var i=0; i<values.length; i++) {
      db.save({_id:(i).toString(), foo:values[i]});
    }

    var queryFun = function(doc) { emit(doc.foo, null); }
    var rows = db.query(queryFun).rows;
    for (i=0; i<values.length; i++) {
      T(equals(rows[i].key, values[i]))
    }

    // everything has collated correctly. Now to check the descending output
    rows = db.query(queryFun, null, {descending: true}).rows
    for (i=0; i<values.length; i++) {
      T(equals(rows[i].key, values[values.length - 1 -i]))
    }

    // now check the key query args
    for (i=1; i<values.length; i++) {
      var queryOptions = {key:values[i]}
      rows = db.query(queryFun, null, queryOptions).rows;
      T(rows.length == 1 && equals(rows[0].key, values[i]))
    }
  },

  view_conflicts: function(debug) {
    var dbA = new CouchDB("test_suite_db_a");
    dbA.deleteDb();
    dbA.createDb();
    var dbB = new CouchDB("test_suite_db_b");
    dbB.deleteDb();
    dbB.createDb();
    if (debug) debugger;

    var docA = {_id: "foo", bar: 42};
    T(dbA.save(docA).ok);
    CouchDB.replicate(dbA.name, dbB.name);

    var docB = dbB.open("foo");
    docB.bar = 43;
    dbB.save(docB);
    docA.bar = 41;
    dbA.save(docA);
    CouchDB.replicate(dbA.name, dbB.name);

    var doc = dbB.open("foo", {conflicts: true});
    T(doc._conflicts.length == 1);
    var conflictRev = doc._conflicts[0];
    if (doc.bar == 41) { // A won
      T(conflictRev == docB._rev);
    } else { // B won
      T(doc.bar == 43);
      T(conflictRev == docA._rev);
    }

    var results = dbB.query(function(doc) {
      if (doc._conflicts) {
        emit(doc._id, doc._conflicts);
      }
    });
    T(results.rows[0].value[0] == conflictRev);
  },

  view_errors: function(debug) {
    var db = new CouchDB("test_suite_db");
    db.deleteDb();
    db.createDb();
    if (debug) debugger;

    var doc = {integer: 1, string: "1", array: [1, 2, 3]};
    T(db.save(doc).ok);

    // emitting a key value that is undefined should result in that row not
    // being included in the view results
    var results = db.query(function(doc) {
      emit(doc.undef, null);
    });
    T(results.total_rows == 0);

    // if a view function throws an exception, its results are not included in
    // the view index, but the view does not itself raise an error
    var results = db.query(function(doc) {
      doc.undef(); // throws an error
    });
    T(results.total_rows == 0);

    // if a view function includes an undefined value in the emitted key or
    // value, an error is logged and the result is not included in the view
    // index, and the view itself does not raise an error
    var results = db.query(function(doc) {
      emit([doc._id, doc.undef], null);
    });
    T(results.total_rows == 0);
  },

  view_include_docs: function(debug) {
    var db = new CouchDB("test_suite_db");
    db.deleteDb();
    db.createDb();
    if (debug) debugger;

    var docs = makeDocs(0, 100);
    T(db.bulkSave(docs).ok);

    var designDoc = {
      _id:"_design/test",
      language: "javascript",
      views: {
        all_docs: {
          map: "function(doc) { emit(doc.integer, doc.string) }"
        },
        with_prev: {
          map: "function(doc){if(doc.prev) emit(doc._id,{'_rev':doc.prev}); else emit(doc._id,{'_rev':doc._rev});}"
        },
        summate: {
          map:"function (doc) {emit(doc.integer, doc.integer)};",
          reduce:"function (keys, values) { return sum(values); };"
        }
      }
    }
    T(db.save(designDoc).ok);

    var resp = db.view('test/all_docs', {include_docs: true, limit: 2});
    T(resp.rows.length == 2);
    T(resp.rows[0].id == "0");
    T(resp.rows[0].doc._id == "0");
    T(resp.rows[1].id == "1");
    T(resp.rows[1].doc._id == "1");

    resp = db.view('test/all_docs', {include_docs: true}, [29, 74]);
    T(resp.rows.length == 2);
    T(resp.rows[0].doc._id == "29");
    T(resp.rows[1].doc.integer == 74);

    resp = db.allDocs({limit: 2, skip: 1, include_docs: true});
    T(resp.rows.length == 2);
    T(resp.rows[0].doc.integer == 1);
    T(resp.rows[1].doc.integer == 10);

    resp = db.allDocs({include_docs: true}, ['not_a_doc']);
    T(resp.rows.length == 1);
    T(!resp.rows[0].doc);

    resp = db.allDocs({include_docs: true}, ["1", "foo"]);
    T(resp.rows.length == 2);
    T(resp.rows[0].doc.integer == 1);
    T(!resp.rows[1].doc);

    resp = db.allDocs({include_docs: true, limit: 0});
    T(resp.rows.length == 0);

    // No reduce support
    try {
        resp = db.view('test/summate', {include_docs: true});
        alert(JSON.stringify(resp));
        T(0==1);
    } catch (e) {
        T(e.error == 'query_parse_error');
    }

    // Reduce support when reduce=false
    resp = db.view('test/summate', {reduce: false, include_docs: true});
    T(resp.rows.length == 100);

    // Check emitted _rev controls things
    resp = db.allDocs({include_docs: true}, ["0"]);
    var before = resp.rows[0].doc;
    var after = db.open("0");
    after.integer = 100
    after.prev = after._rev;
    db.save(after);
    after = db.open("0");
    T(after._rev != after.prev);
    T(after.integer == 100);

    // should emit the previous revision
    resp = db.view("test/with_prev", {include_docs: true}, ["0"]);
    T(resp.rows[0].doc._id == "0");
    T(resp.rows[0].doc._rev == before._rev);
    T(!resp.rows[0].doc.prev);
    T(resp.rows[0].doc.integer == 0);

    var xhr = CouchDB.request("POST", "/test_suite_db/_compact");
    T(xhr.status == 202)
    while (db.info().compact_running) {}

    resp = db.view("test/with_prev", {include_docs: true}, ["0", "23"]);
    T(resp.rows.length == 2);
    T(resp.rows[0].key == "0");
    T(resp.rows[0].id == "0");
    T(!resp.rows[0].doc);
    T(resp.rows[0].error == "missing");
    T(resp.rows[1].doc.integer == 23);
  },

  view_multi_key_all_docs: function(debug) {
    var db = new CouchDB("test_suite_db");
    db.deleteDb();
    db.createDb();
    if (debug) debugger;

    var docs = makeDocs(0, 100);
    T(db.bulkSave(docs).ok);

    var keys = ["10","15","30","37","50"];
    var rows = db.allDocs({},keys).rows;
    T(rows.length == keys.length);
    for(var i=0; i<rows.length; i++)
      T(rows[i].id == keys[i]);

    rows = db.allDocs({limit: 1}, keys).rows;
    T(rows.length == 1);
    T(rows[0].id == keys[0]);

    rows = db.allDocs({skip: 2}, keys).rows;
    T(rows.length == 3);
    for(var i=0; i<rows.length; i++)
        T(rows[i].id == keys[i+2]);

    rows = db.allDocs({descending: "true"}, keys).rows;
    T(rows.length == keys.length);
    for(var i=0; i<rows.length; i++)
        T(rows[i].id == keys[keys.length-i-1]);

    rows = db.allDocs({descending: "true", skip: 3, limit:1}, keys).rows;
    T(rows.length == 1);
    T(rows[0].id == keys[1]);

    // Check we get invalid rows when the key doesn't exist
    rows = db.allDocs({}, [1, "i_dont_exist", "0"]).rows;
    T(rows.length == 3);
    T(rows[0].error == "not_found");
    T(!rows[0].id);
    T(rows[1].error == "not_found");
    T(!rows[1].id);
    T(rows[2].id == rows[2].key && rows[2].key == "0");
  },

  view_multi_key_design: function(debug) {
    var db = new CouchDB("test_suite_db");
    db.deleteDb();
    db.createDb();
    if (debug) debugger;

    var docs = makeDocs(0, 100);
    T(db.bulkSave(docs).ok);

    var designDoc = {
      _id:"_design/test",
      language: "javascript",
      views: {
        all_docs: {
          map: "function(doc) { emit(doc.integer, doc.string) }"
        },
        multi_emit: {
          map: "function(doc) {for(var i = 0 ; i < 3 ; i++) { emit(i, doc.integer) ; } }"
        },
        summate: {
          map:"function (doc) {emit(doc.integer, doc.integer)};",
          reduce:"function (keys, values) { return sum(values); };"
        }
      }
    }
    T(db.save(designDoc).ok);

    // Test that missing keys work too
    var keys = [101,30,15,37,50]
    var reduce = db.view("test/summate",{group:true},keys).rows;
    T(reduce.length == keys.length-1); // 101 is missing
    for(var i=0; i<reduce.length; i++) {
      T(keys.indexOf(reduce[i].key) != -1);
      T(reduce[i].key == reduce[i].value);
    }

    // First, the goods:
    var keys = [10,15,30,37,50];
    var rows = db.view("test/all_docs",{},keys).rows;
    for(var i=0; i<rows.length; i++) {
      T(keys.indexOf(rows[i].key) != -1);
      T(rows[i].key == rows[i].value);
    }
    
    var reduce = db.view("test/summate",{group:true},keys).rows;
    T(reduce.length == keys.length);
    for(var i=0; i<reduce.length; i++) {
      T(keys.indexOf(reduce[i].key) != -1);
      T(reduce[i].key == reduce[i].value);
    }

    // Test that invalid parameter combinations get rejected
    var badargs = [{startkey:0}, {endkey:0}, {key: 0}, {group_level: 2}];
    for(var i in badargs)
    {
        try {
            db.view("test/all_docs",badargs[i],keys);
            T(0==1);
        } catch (e) {
            T(e.error == "query_parse_error");
        }
    }

    try {
        db.view("test/summate",{},keys);
        T(0==1);
    } catch (e) {
        T(e.error == "query_parse_error");
    }

    // Test that a map & reduce containing func support keys when reduce=false
    resp = db.view("test/summate", {reduce: false}, keys);
    T(resp.rows.length == 5);

    // Check that limiting by startkey_docid and endkey_docid get applied
    // as expected.
    var curr = db.view("test/multi_emit", {startkey_docid: 21, endkey_docid: 23}, [0, 2]).rows;
    var exp_key = [ 0,  0,  0,  2,  2,  2] ;
    var exp_val = [21, 22, 23, 21, 22, 23] ;
    T(curr.length == 6);
    for( var i = 0 ; i < 6 ; i++)
    {
        T(curr[i].key == exp_key[i]);
        T(curr[i].value == exp_val[i]);
    }

    // Check limit works
    curr = db.view("test/all_docs", {limit: 1}, keys).rows;
    T(curr.length == 1);
    T(curr[0].key == 10);

    // Check offset works
    curr = db.view("test/multi_emit", {skip: 1}, [0]).rows;
    T(curr.length == 99);
    T(curr[0].value == 1);

    // Check that dir works
    curr = db.view("test/multi_emit", {descending: "true"}, [1]).rows;
    T(curr.length == 100);
    T(curr[0].value == 99);
    T(curr[99].value == 0);

    // Check a couple combinations
    curr = db.view("test/multi_emit", {descending: "true", skip: 3, limit: 2}, [2]).rows;
    T(curr.length, 2);
    T(curr[0].value == 96);
    T(curr[1].value == 95);

    curr = db.view("test/multi_emit", {skip: 2, limit: 3, startkey_docid: "13"}, [0]).rows;
    T(curr.length == 3);
    T(curr[0].value == 15);
    T(curr[1].value == 16);
    T(curr[2].value == 17);

    curr = db.view("test/multi_emit",
            {skip: 1, limit: 5, startkey_docid: "25", endkey_docid: "27"}, [1]).rows;
    T(curr.length == 2);
    T(curr[0].value == 26);
    T(curr[1].value == 27);

    curr = db.view("test/multi_emit",
            {skip: 1, limit: 5, startkey_docid: "28", endkey_docid: "26", descending: "true"}, [1]).rows;
    T(curr.length == 2);
    T(curr[0].value == 27);
    T(curr[1].value == 26);
  },

  view_multi_key_temp: function(debug) {
    var db = new CouchDB("test_suite_db");
    db.deleteDb();
    db.createDb();
    if (debug) debugger;

    var docs = makeDocs(0, 100);
    T(db.bulkSave(docs).ok);

    var queryFun = function(doc) { emit(doc.integer, doc.integer) };
    var reduceFun = function (keys, values) { return sum(values); };

    var keys = [10,15,30,37,50];
    var rows = db.query(queryFun, null, {}, keys).rows;
    for(var i=0; i<rows.length; i++) {
      T(keys.indexOf(rows[i].key) != -1);
      T(rows[i].key == rows[i].value);
    }
    
    var reduce = db.query(queryFun, reduceFun, {group:true}, keys).rows;
    for(var i=0; i<reduce.length; i++) {
      T(keys.indexOf(reduce[i].key) != -1);
      T(reduce[i].key == reduce[i].value);
    }
  },

  view_pagination: function(debug) {
    var db = new CouchDB("test_suite_db");
    db.deleteDb();
    db.createDb();
    if (debug) debugger;

    var docs = makeDocs(0, 100);
    T(db.bulkSave(docs).ok);

    var queryFun = function(doc) { emit(doc.integer, null) };
    var i;

    // page through the view ascending
    for (i = 0; i < docs.length; i += 10) {
      var queryResults = db.query(queryFun, null, {
        startkey: i,
        startkey_docid: i,
        limit: 10
      });
      T(queryResults.rows.length == 10)
      T(queryResults.total_rows == docs.length)
      T(queryResults.offset == i)
      var j;
      for (j = 0; j < 10;j++) {
        T(queryResults.rows[j].key == i + j);
      }
    }

    // page through the view descending
    for (i = docs.length - 1; i >= 0; i -= 10) {
      var queryResults = db.query(queryFun, null, {
        startkey: i,
        startkey_docid: i,
        descending: true,
        limit: 10
      });
      T(queryResults.rows.length == 10)
      T(queryResults.total_rows == docs.length)
      T(queryResults.offset == docs.length - i - 1)
      var j;
      for (j = 0; j < 10; j++) {
        T(queryResults.rows[j].key == i - j);
      }
    }

    // ignore decending=false. CouchDB should just ignore that.
    for (i = 0; i < docs.length; i += 10) {
      var queryResults = db.query(queryFun, null, {
        startkey: i,
        startkey_docid: i,
        descending: false,
        limit: 10
      });
      T(queryResults.rows.length == 10)
      T(queryResults.total_rows == docs.length)
      T(queryResults.offset == i)
      var j;
      for (j = 0; j < 10;j++) {
        T(queryResults.rows[j].key == i + j);
      }
    }
    
    // test endkey_docid
    var queryResults = db.query(function(doc) { emit(null, null);}, null, {
      startkey: null,
      startkey_docid: 1,
      endkey: null,
      endkey_docid: 40
    });
    
    T(queryResults.rows.length == 35)
    T(queryResults.total_rows == docs.length)
    T(queryResults.offset == 1)
    T(queryResults.rows[0].id == "1");
    T(queryResults.rows[1].id == "10");
    T(queryResults.rows[2].id == "11");
    T(queryResults.rows[3].id == "12");
    T(queryResults.rows[4].id == "13");
    T(queryResults.rows[5].id == "14");
    T(queryResults.rows[6].id == "15");
    T(queryResults.rows[7].id == "16");
    T(queryResults.rows[8].id == "17");
    T(queryResults.rows[9].id == "18");
    T(queryResults.rows[10].id == "19");
    T(queryResults.rows[11].id == "2");
    T(queryResults.rows[12].id == "20");
    T(queryResults.rows[13].id == "21");
    T(queryResults.rows[14].id == "22");
    T(queryResults.rows[15].id == "23");
    T(queryResults.rows[16].id == "24");
    T(queryResults.rows[17].id == "25");
    T(queryResults.rows[18].id == "26");
    T(queryResults.rows[19].id == "27");
    T(queryResults.rows[20].id == "28");
    T(queryResults.rows[21].id == "29");
    T(queryResults.rows[22].id == "3");
    T(queryResults.rows[23].id == "30");
    T(queryResults.rows[24].id == "31");
    T(queryResults.rows[25].id == "32");
    T(queryResults.rows[26].id == "33");
    T(queryResults.rows[27].id == "34");
    T(queryResults.rows[28].id == "35");
    T(queryResults.rows[29].id == "36");
    T(queryResults.rows[30].id == "37");
    T(queryResults.rows[31].id == "38");
    T(queryResults.rows[32].id == "39");
    T(queryResults.rows[33].id == "4");
    T(queryResults.rows[34].id == "40");

  },

  view_sandboxing: function(debug) {
    var db = new CouchDB("test_suite_db");
    db.deleteDb();
    db.createDb();
    if (debug) debugger;

    var doc = {integer: 1, string: "1", array: [1, 2, 3]};
    T(db.save(doc).ok);
/*
    // make sure that attempting to change the document throws an error
    var results = db.query(function(doc) {
      doc.integer = 2;
      emit(null, doc);
    });
    T(results.total_rows == 0);

    var results = db.query(function(doc) {
      doc.array[0] = 0;
      emit(null, doc);
    });
    T(results.total_rows == 0);
*/
    // make sure that a view cannot invoke interpreter internals such as the
    // garbage collector
    var results = db.query(function(doc) {
      gc();
      emit(null, doc);
    });
    T(results.total_rows == 0);

    // make sure that a view cannot access the map_funs array defined used by
    // the view server
    var results = db.query(function(doc) { map_funs.push(1); emit(null, doc) });
    T(results.total_rows == 0);

    // make sure that a view cannot access the map_results array defined used by
    // the view server
    var results = db.query(function(doc) { map_results.push(1); emit(null, doc) });
    T(results.total_rows == 0);
  },

  view_xml: function(debug) {
    var db = new CouchDB("test_suite_db");
    db.deleteDb();
    db.createDb();
    if (debug) debugger;

    db.save({content: "<doc><title id='xml'>Testing XML</title></doc>"});
    db.save({content: "<doc><title id='e4x'>Testing E4X</title></doc>"});

    var results = db.query(
      "function(doc) {\n" +
      "  var xml = new XML(doc.content);\n" +
      "  emit(xml.title.text(), null);\n" +
      "}");
    T(results.total_rows == 2);
    T(results.rows[0].key == "Testing E4X");
    T(results.rows[1].key == "Testing XML");

    var results = db.query(
      "function(doc) {\n" +
      "  var xml = new XML(doc.content);\n" +
      "  emit(xml.title.@id, null);\n" +
      "}");
    T(results.total_rows == 2);
    T(results.rows[0].key == "e4x");
    T(results.rows[1].key == "xml");
  },

  replication: function(debug) {
    if (debug) debugger;
    var host = CouchDB.host;
    var dbPairs = [
      {source:"test_suite_db_a",
        target:"test_suite_db_b"},
      {source:"test_suite_db_a",
        target:"http://" + host + "/test_suite_db_b"},
      {source:"http://" + host + "/test_suite_db_a",
        target:"test_suite_db_b"},
      {source:"http://" + host + "/test_suite_db_a",
        target:"http://" + host + "/test_suite_db_b"}
    ]
    var dbA = new CouchDB("test_suite_db_a");
    var dbB = new CouchDB("test_suite_db_b");
    var numDocs = 10;
    var xhr;
    for (var testPair = 0; testPair < dbPairs.length; testPair++) {
      var A = dbPairs[testPair].source
      var B = dbPairs[testPair].target

      dbA.deleteDb();
      dbA.createDb();
      dbB.deleteDb();
      dbB.createDb();
      
      var repTests = {
        // copy and paste and put your code in. delete unused steps.
        test_template: new function () {
          this.init = function(dbA, dbB) {
            // before anything has happened
          }
          this.afterAB1 = function(dbA, dbB) {
            // called after replicating src=A  tgt=B first time.
          };
          this.afterBA1 = function(dbA, dbB) {
            // called after replicating src=B  tgt=A first time.
          };
          this.afterAB2 = function(dbA, dbB) {
            // called after replicating src=A  tgt=B second time. 
          };
          this.afterBA2 = function(dbA, dbB) {
            // etc...
          };
        },
        
        simple_test: new function () {
          this.init = function(dbA, dbB) {
            var docs = makeDocs(0, numDocs);
            T(dbA.bulkSave(docs).ok);
          };
        
          this.afterAB1 = function(dbA, dbB) {          
            for (var j = 0; j < numDocs; j++) {
              var docA = dbA.open("" + j);
              var docB = dbB.open("" + j);
              T(docA._rev == docB._rev);
            }
          };
        },
      
       deletes_test: new function () {
          this.init = function(dbA, dbB) {
            T(dbA.save({_id:"foo1",value:"a"}).ok);
          };
          
          this.afterAB1 = function(dbA, dbB) {
            var docA = dbA.open("foo1");
            var docB = dbB.open("foo1");
            T(docA._rev == docB._rev);

            dbA.deleteDoc(docA);
          };
          
          this.afterAB2 = function(dbA, dbB) {
            T(dbA.open("foo1") == null);
            T(dbB.open("foo1") == null);
          };
        },
        
        slashes_in_ids_test: new function () {
          // make sure docs with slashes in id replicate properly
          this.init = function(dbA, dbB) {
            dbA.save({ _id:"abc/def", val:"one" });
          };
          
          this.afterAB1 = function(dbA, dbB) {
            var docA = dbA.open("abc/def");
            var docB = dbB.open("abc/def");
            T(docA._rev == docB._rev);
          };
        },
      
        attachments_test: new function () {
          // Test attachments
          this.init = function(dbA, dbB) {
            dbA.save({
              _id:"bin_doc",
              _attachments:{
                "foo.txt": {
                  "type":"base64",
                  "data": "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ="
                }
              }
            });
          };
          
          this.afterAB1 = function(dbA, dbB) {
            var xhr = CouchDB.request("GET", "/test_suite_db_a/bin_doc/foo.txt");
            T(xhr.responseText == "This is a base64 encoded text")

            xhr = CouchDB.request("GET", "/test_suite_db_b/bin_doc/foo.txt");
            T(xhr.responseText == "This is a base64 encoded text")
          };
        },
        
        conflicts_test: new function () {
          // test conflicts
          this.init = function(dbA, dbB) {
            dbA.save({_id:"foo",value:"a"});
            dbB.save({_id:"foo",value:"b"});
          };
          
          this.afterBA1 = function(dbA, dbB) {            
            var docA = dbA.open("foo", {conflicts: true});
            var docB = dbB.open("foo", {conflicts: true});

            // make sure the same rev is in each db
            T(docA._rev === docB._rev);

            // make sure the conflicts are the same in each db
            T(docA._conflicts[0] === docB._conflicts[0]);

            // delete a conflict.
            dbA.deleteDoc({_id:"foo", _rev:docA._conflicts[0]});
          };
          
          this.afterBA2 = function(dbA, dbB) {            
            // open documents and include the conflict meta data
            var docA = dbA.open("foo", {conflicts: true});
            var docB = dbB.open("foo", {conflicts: true});

            // We should have no conflicts this time
            T(docA._conflicts === undefined)
            T(docB._conflicts === undefined);
          };
        }
      };

      var test;
      for(test in repTests) {
        if(repTests[test].init) {
          repTests[test].init(dbA, dbB);
        }
      }

      T(CouchDB.replicate(A, B).ok);

      for(test in repTests) {
        if(repTests[test].afterAB1) repTests[test].afterAB1(dbA, dbB);
      }

      T(CouchDB.replicate(B, A).ok);

      for(test in repTests) {
        if(repTests[test].afterBA1) repTests[test].afterBA1(dbA, dbB);
      }

      T(CouchDB.replicate(A, B).ok);

      for(test in repTests) {
        if(repTests[test].afterAB2) repTests[test].afterAB2(dbA, dbB);
      }

      T(CouchDB.replicate(B, A).ok);

      for(test in repTests) {
        if(repTests[test].afterBA2) repTests[test].afterBA2(dbA, dbB);
      }

    }
  },

  etags_head: function(debug) {
    var db = new CouchDB("test_suite_db");
    db.deleteDb();
    db.createDb();
    if (debug) debugger;

    var xhr;

    // create a new doc
    xhr = CouchDB.request("PUT", "/test_suite_db/1", {
      body: "{}"
    });
    T(xhr.status == 201);

    // extract the ETag header values
    var etag = xhr.getResponseHeader("etag")

    // get the doc and verify the headers match
    xhr = CouchDB.request("GET", "/test_suite_db/1");
    T(etag == xhr.getResponseHeader("etag"));

    // 'head' the doc and verify the headers match
    xhr = CouchDB.request("HEAD", "/test_suite_db/1", {
      headers: {"if-none-match": "s"}
    });
    T(etag == xhr.getResponseHeader("etag"));

    // replace a doc
    xhr = CouchDB.request("PUT", "/test_suite_db/1", {
      body: "{}",
      headers: {"if-match": etag}
    });
    T(xhr.status == 201);

    // extract the new ETag value
    var etagOld= etag;
    etag = xhr.getResponseHeader("etag")

    // fail to replace a doc
    xhr = CouchDB.request("PUT", "/test_suite_db/1", {
      body: "{}"
    });
    T(xhr.status == 412)

    // verify get w/Etag
    xhr = CouchDB.request("GET", "/test_suite_db/1", {
      headers: {"if-none-match": etagOld}
    });
    T(xhr.status == 200);
    xhr = CouchDB.request("GET", "/test_suite_db/1", {
      headers: {"if-none-match": etag}
    });
    T(xhr.status == 304);

    // fail to delete a doc
    xhr = CouchDB.request("DELETE", "/test_suite_db/1", {
      headers: {"if-match": etagOld}
    });
    T(xhr.status == 412);

    //now do it for real
    xhr = CouchDB.request("DELETE", "/test_suite_db/1", {
      headers: {"if-match": etag}
    });
    T(xhr.status == 200)
  },

   show_documents: function(debug) {
     var db = new CouchDB("test_suite_db");
     db.deleteDb();
     db.createDb();
     if (debug) debugger;
     
     function stringFun(fun) {
       var string = fun.toSource ? fun.toSource() : "(" + fun.toString() + ")";
       return string;
     }
         
     var designDoc = {
       _id:"_design/template",
       language: "javascript",
       shows: {
         "hello" : stringFun(function() { 
           return {
             body : "Hello World"
           };
         }),
         "just-name" : stringFun(function(doc, req) {
           return {
             body : "Just " + doc.name
           };
         }),
         "req-info" : stringFun(function(doc, req) {
           return {
             json : req
           }
         }),
         "xml-type" : stringFun(function(doc, req) {
            return {
              "headers" : {
                "Content-Type" : "application/xml"
              },
              "body" : new XML('<xml><node foo="bar"/></xml>')
            }
          }),
         "no-set-etag" : stringFun(function(doc, req) {
           return {
             headers : {
               "Etag" : "skipped"
             },
             "body" : "something"
           }
         }),
         "accept-switch" : stringFun(function(doc, req) {
           if (req.headers["Accept"].match(/image/)) {
             return {
               // a 16x16 px version of the CouchDB logo
               "base64" : 
["iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAMAAAAoLQ9TAAAAsV",
"BMVEUAAAD////////////////////////5ur3rEBn////////////////wDBL/",
"AADuBAe9EB3IEBz/7+//X1/qBQn2AgP/f3/ilpzsDxfpChDtDhXeCA76AQH/v7",
"/84eLyWV/uc3bJPEf/Dw/uw8bRWmP1h4zxSlD6YGHuQ0f6g4XyQkXvCA36MDH6",
"wMH/z8/yAwX64ODeh47BHiv/Ly/20dLQLTj98PDXWmP/Pz//39/wGyJ7Iy9JAA",
"AADHRSTlMAbw8vf08/bz+Pv19jK/W3AAAAg0lEQVR4Xp3LRQ4DQRBD0QqTm4Y5",
"zMxw/4OleiJlHeUtv2X6RbNO1Uqj9g0RMCuQO0vBIg4vMFeOpCWIWmDOw82fZx",
"vaND1c8OG4vrdOqD8YwgpDYDxRgkSm5rwu0nQVBJuMg++pLXZyr5jnc1BaH4GT",
"LvEliY253nA3pVhQqdPt0f/erJkMGMB8xucAAAAASUVORK5CYII="].join(''),
               headers : {
                 "Content-Type" : "image/png",
                 "Vary" : "Accept" // we set this for proxy caches
               }
             };
           } else {
             return {
               "body" : "accepting text requests",
               headers : {
                 "Content-Type" : "text/html",
                 "Vary" : "Accept"
               }
             };
           }
         }),
         "respondWith" : stringFun(function(doc, req) {
           registerType("foo", "application/foo","application/x-foo");
           return respondWith(req, {
             html : function() {
               return {
                 body:"Ha ha, you said \"" + doc.word + "\"."
               };
             },
             xml : function() {
               var xml = new XML('<xml><node/></xml>');
               // Becase Safari can't stand to see that dastardly
               // E4X outside of a string. Outside of tests you
               // can just use E4X literals.
               this.eval('xml.node.@foo = doc.word');
               return {
                 body: xml
               };
             },
             foo : function() {
               return {
                 body: "foofoo"
               };
             },
             fallback : "html"
           });
         })
       }
     };
     T(db.save(designDoc).ok);
     
     var doc = {"word":"plankton", "name":"Rusty"}
     var resp = db.save(doc);
     T(resp.ok);
     var docid = resp.id;
 
     // show error
     var xhr = CouchDB.request("GET", "/test_suite_db/_show/");
     T(xhr.status == 404);
     T(JSON.parse(xhr.responseText).reason == "Invalid path.");
 
     // hello template world
     xhr = CouchDB.request("GET", "/test_suite_db/_show/template/hello/"+docid);
     T(xhr.responseText == "Hello World");
     
     // show with doc
     xhr = CouchDB.request("GET", "/test_suite_db/_show/template/just-name/"+docid);
     T(xhr.responseText == "Just Rusty");
     
     // show with missing doc
     xhr = CouchDB.request("GET", "/test_suite_db/_show/template/just-name/missingdoc");
     T(xhr.status == 404);
     var resp = JSON.parse(xhr.responseText);
     T(resp.error == "not_found");
     T(resp.reason == "missing");
     
     // show with missing func
     xhr = CouchDB.request("GET", "/test_suite_db/_show/template/missing/"+docid);
     T(xhr.status == 404);
     
     // missing design doc
     xhr = CouchDB.request("GET", "/test_suite_db/_show/missingdoc/just-name/"+docid);
     T(xhr.status == 404);
     var resp = JSON.parse(xhr.responseText);
     T(resp.error == "not_found");
     
     // query parameters
     xhr = CouchDB.request("GET", "/test_suite_db/_show/template/req-info/"+docid+"?foo=bar", {
       headers: {
         "Accept": "text/html;text/plain;*/*",
         "X-Foo" : "bar"
       }
     });
     var resp = JSON.parse(xhr.responseText);
     T(equals(resp.headers["X-Foo"], "bar"));
     T(equals(resp.query, {foo:"bar"}));
     T(equals(resp.verb, "GET"));
     T(equals(resp.path[4], docid));
     T(equals(resp.info.db_name, "test_suite_db"));
     
     // returning a content-type
     xhr = CouchDB.request("GET", "/test_suite_db/_show/template/xml-type/"+docid);
     T("application/xml" == xhr.getResponseHeader("Content-Type"));
     T("Accept" == xhr.getResponseHeader("Vary"));
 
     // accept header switching
     // different mime has different etag
     
     xhr = CouchDB.request("GET", "/test_suite_db/_show/template/accept-switch/"+docid, {
       headers: {"Accept": "text/html;text/plain;*/*"}
     });
     T("text/html" == xhr.getResponseHeader("Content-Type"));
     T("Accept" == xhr.getResponseHeader("Vary"));
     var etag = xhr.getResponseHeader("etag");

     xhr = CouchDB.request("GET", "/test_suite_db/_show/template/accept-switch/"+docid, {
       headers: {"Accept": "image/png;*/*"}
     });
     T(xhr.responseText.match(/PNG/))
     T("image/png" == xhr.getResponseHeader("Content-Type"));
     var etag2 = xhr.getResponseHeader("etag");
     T(etag2 != etag);
 
     // proper etags
     // show with doc
     xhr = CouchDB.request("GET", "/test_suite_db/_show/template/just-name/"+docid);
     // extract the ETag header values
     etag = xhr.getResponseHeader("etag");
     // get again with etag in request
     xhr = CouchDB.request("GET", "/test_suite_db/_show/template/just-name/"+docid, {
       headers: {"if-none-match": etag}
     });
     // should be 304
     T(xhr.status == 304);    
 
     // update the doc
     doc.name = "Crusty";
     resp = db.save(doc);
     T(resp.ok);
     // req with same etag
     xhr = CouchDB.request("GET", "/test_suite_db/_show/template/just-name/"+docid, {
       headers: {"if-none-match": etag}
     });
     // status is 200    
     T(xhr.status == 200);
 
     // get new etag and request again
     etag = xhr.getResponseHeader("etag");
     xhr = CouchDB.request("GET", "/test_suite_db/_show/template/just-name/"+docid, {
       headers: {"if-none-match": etag}
     });
     // should be 304
     T(xhr.status == 304);
 
     // update design doc (but not function)
     designDoc.isChanged = true;
     T(db.save(designDoc).ok);
     
     xhr = CouchDB.request("GET", "/test_suite_db/_show/template/just-name/"+docid, {
       headers: {"if-none-match": etag}
     });
     // should be 304
     T(xhr.status == 304);
     
     // update design doc function
     designDoc.shows["just-name"] = (function(doc, req) {
       return {
         body : "Just old " + doc.name
       };
     }).toString();
     T(db.save(designDoc).ok);
 
     xhr = CouchDB.request("GET", "/test_suite_db/_show/template/just-name/"+docid, {
       headers: {"if-none-match": etag}
     });
     // status is 200    
     T(xhr.status == 200);
     
     
     // JS can't set etag
     xhr = CouchDB.request("GET", "/test_suite_db/_show/template/no-set-etag/"+docid);
     // extract the ETag header values
     etag = xhr.getResponseHeader("etag");
     T(etag != "skipped")
 
     // test the respondWith mime matcher
     xhr = CouchDB.request("GET", "/test_suite_db/_show/template/respondWith/"+docid, {
       headers: {
         "Accept": 'text/html,application/atom+xml; q=0.9'
       }
     });
     T(xhr.getResponseHeader("Content-Type") == "text/html");
     T(xhr.responseText == "Ha ha, you said \"plankton\".");
 
     // now with xml
     xhr = CouchDB.request("GET", "/test_suite_db/_show/template/respondWith/"+docid, {
       headers: {
         "Accept": 'application/xml'
       }
     });
     T(xhr.getResponseHeader("Content-Type") == "application/xml");
     T(xhr.responseText.match(/node/));
     T(xhr.responseText.match(/plankton/));
     
     // registering types works
     xhr = CouchDB.request("GET", "/test_suite_db/_show/template/respondWith/"+docid, {
       headers: {
         "Accept": "application/x-foo"
       }
     });
     T(xhr.getResponseHeader("Content-Type") == "application/x-foo");
     T(xhr.responseText.match(/foofoo/));
   },

  list_views : function(debug) {
    var db = new CouchDB("test_suite_db");
    db.deleteDb();
    db.createDb();
    if (debug) debugger;
    
    function stringFun(fun) {
      var string = fun.toSource ? fun.toSource() : "(" + fun.toString() + ")";
      return string;
    }
        
    var designDoc = {
      _id:"_design/lists",
      language: "javascript",
      views : {
        basicView : {
          map : stringFun(function(doc) {
            emit(doc.integer, doc.string);
          })
        },
        withReduce : {
          map : stringFun(function(doc) {
            emit(doc.integer, doc.string);
          }),
          reduce : stringFun(function(keys, values, rereduce) {
            if (rereduce) {
              return sum(values);
            } else {
              return values.length;
            }
          })
        }
      },
      lists: {
        simpleForm: stringFun(function(head, row, req) {
          if (row) {
            // we ignore headers on rows and tail
            return {body : '\n<li>Key: '+row.key+' Value: '+row.value+'</li>'};
          } else if (head) {
            // we return an object (like those used by external and show)
            // so that we can specify headers
            return {
              body : '<h1>Total Rows: '
                + head.total_rows
                + ' Offset: ' + head.offset
                + '</h1><ul>'
            };
          } else {
            // tail
            return {body : '</ul>'};
          }
        }),
        acceptSwitch: stringFun(function(head, row, req) {
          return respondWith(req, {
            html : function() {
              if (head) {
                return {body : "HTML <ul>"};
              } else if (row) {
                return {body : '\n<li>Key: '
                  +row.key+' Value: '+row.value+'</li>'};
              } else { // tail
                return {body : "</ul>"};
              }
            },
            xml : function() {
              if (head) {
                return {body:'<feed xmlns="http://www.w3.org/2005/Atom">'
                  +'<title>Test XML Feed</title>'};
              } else if (row) {
                // Becase Safari can't stand to see that dastardly
                // E4X outside of a string. Outside of tests you
                // can just use E4X literals.
                var entry = new XML('<entry/>');
                entry.id = row.id;
                entry.title = row.key;
                entry.content = row.value;
                return {body:entry};
              } else {
                return {body : "</feed>"};
              }
            }
          })
        })
      }
    };

    T(db.save(designDoc).ok);
    
    var docs = makeDocs(0, 10);
    var saveResult = db.bulkSave(docs);
    T(saveResult.ok);
    
    var view = db.view('lists/basicView');
    T(view.total_rows == 10);
    
    // standard get
    var xhr = CouchDB.request("GET", "/test_suite_db/_list/lists/simpleForm/basicView");
    T(xhr.status == 200);
    T(/Total Rows/.test(xhr.responseText));
    T(/Key: 1/.test(xhr.responseText));
    
    // get with query params
    var xhr = CouchDB.request("GET", "/test_suite_db/_list/lists/simpleForm/basicView?startkey=3");
    T(xhr.status == 200);
    T(/Total Rows/.test(xhr.responseText));
    T(!(/Key: 1/.test(xhr.responseText)));
    
    // with 0 rows
    var xhr = CouchDB.request("GET", "/test_suite_db/_list/lists/simpleForm/basicView?startkey=30");
    T(xhr.status == 200);
    T(/Total Rows/.test(xhr.responseText));
    T(/Offset: null/.test(xhr.responseText));
    
    // when there is a reduce present, but not used
    var xhr = CouchDB.request("GET", "/test_suite_db/_list/lists/simpleForm/withReduce?reduce=false");
    T(xhr.status == 200);
    T(/Total Rows/.test(xhr.responseText));
    T(/Key: 1/.test(xhr.responseText));
    
    // with accept headers for HTML
    xhr = CouchDB.request("GET", "/test_suite_db/_list/lists/acceptSwitch/basicView", {
      headers: {
        "Accept": 'text/html'
      }
    });
    T(xhr.getResponseHeader("Content-Type") == "text/html");
    T(xhr.responseText.match(/HTML/));
    T(xhr.responseText.match(/Value/));

    // now with xml
    xhr = CouchDB.request("GET", "/test_suite_db/_list/lists/acceptSwitch/basicView", {
      headers: {
        "Accept": 'application/xml'
      }
    });
    T(xhr.getResponseHeader("Content-Type") == "application/xml");
    T(xhr.responseText.match(/XML/));
    T(xhr.responseText.match(/entry/));
  },

  compact: function(debug) {
    var db = new CouchDB("test_suite_db");
    db.deleteDb();
    db.createDb();
    if (debug) debugger;
    var docs = makeDocs(0, 10);
    var saveResult = db.bulkSave(docs);
    T(saveResult.ok);

    var binAttDoc = {
      _id: "bin_doc",
      _attachments:{
        "foo.txt": {
          content_type:"text/plain",
          data: "VGhpcyBpcyBhIGJhc2U2NCBlbmNvZGVkIHRleHQ="
        }
      }
    }

    T(db.save(binAttDoc).ok);

    var originalsize = db.info().disk_size;

    for(var i in docs) {
        db.deleteDoc(docs[i]);
    }
    db.setAdmins(["Foo bar"]);
    var deletesize = db.info().disk_size;
    T(deletesize > originalsize);

    var xhr = CouchDB.request("POST", "/test_suite_db/_compact");
    T(xhr.status == 202);
    // compaction isn't instantaneous, loop until done
    while (db.info().compact_running) {};
    
    T(db.ensureFullCommit().ok);
    restartServer();
    var xhr = CouchDB.request("GET", "/test_suite_db/bin_doc/foo.txt");
    T(xhr.responseText == "This is a base64 encoded text")
    T(xhr.getResponseHeader("Content-Type") == "text/plain")
    T(db.info().doc_count == 1);
    T(db.info().disk_size < deletesize);
    
  },
  
  purge: function(debug) {
    var db = new CouchDB("test_suite_db");
    db.deleteDb();
    db.createDb();
    if (debug) debugger;
    
    /*
     purge is not to be confused with a document deletion.  It removes the
     document and all edit history from the local instance of the database.
    */

    var numDocs = 10;

    var designDoc = {
      _id:"_design/test",
      language: "javascript",
      views: {
        all_docs_twice: {map: "function(doc) { emit(doc.integer, null); emit(doc.integer, null) }"},
        single_doc: {map: "function(doc) { if (doc._id == \"1\") { emit(1, null) }}"}
      }
    }
    
    T(db.save(designDoc).ok);

    T(db.bulkSave(makeDocs(1, numDocs + 1)).ok);

    // go ahead and validate the views before purging
    var rows = db.view("test/all_docs_twice").rows;
    for (var i = 0; i < numDocs; i++) {
      T(rows[2*i].key == i+1);
      T(rows[(2*i)+1].key == i+1);
    }
    T(db.view("test/single_doc").total_rows == 1);
    
    var info = db.info();
    var doc1 = db.open("1");
    var doc2 = db.open("2");
    
    // purge the documents
    var xhr = CouchDB.request("POST", "/test_suite_db/_purge", {
      body: JSON.stringify({"1":[doc1._rev], "2":[doc2._rev]}),
    });
    T(xhr.status == 200);

    var newInfo = db.info();
    // purging increments the update sequence
    T(info.update_seq+1 == newInfo.update_seq);
    // and it increments the purge_seq
    T(info.purge_seq+1 == newInfo.purge_seq);

    var result = JSON.parse(xhr.responseText);
    T(result.purged["1"][0] == doc1._rev);
    T(result.purged["2"][0] == doc2._rev);
    
    T(db.open("1") == null);
    T(db.open("2") == null);
    
    var rows = db.view("test/all_docs_twice").rows;
    for (var i = 2; i < numDocs; i++) {
      T(rows[2*(i-2)].key == i+1);
      T(rows[(2*(i-2))+1].key == i+1);
    }
    T(db.view("test/single_doc").total_rows == 0);
    
    // purge documents twice in a row without loading views
    // (causes full view rebuilds)
    
    var doc3 = db.open("3");
    var doc4 = db.open("4");
    
    xhr = CouchDB.request("POST", "/test_suite_db/_purge", {
      body: JSON.stringify({"3":[doc3._rev]}),
    });
    
    T(xhr.status == 200);
    
    xhr = CouchDB.request("POST", "/test_suite_db/_purge", {
      body: JSON.stringify({"4":[doc4._rev]}),
    });
    
    T(xhr.status == 200);
    
    var rows = db.view("test/all_docs_twice").rows;
    for (var i = 4; i < numDocs; i++) {
      T(rows[2*(i-4)].key == i+1);
      T(rows[(2*(i-4))+1].key == i+1);
    }
    T(db.view("test/single_doc").total_rows == 0);
  },
  
  config : function(debug) {
    var db = new CouchDB("test_suite_db");
    db.deleteDb();
    db.createDb();
    if (debug) debugger;
    
    // test that /_config returns all the settings
    var xhr = CouchDB.request("GET", "/_config");
    var config = JSON.parse(xhr.responseText);
    var port = CouchDB.host.split(':').pop()
    T(config.couchdb.database_dir);
    T(config.httpd.port == port);
    T(config.daemons.httpd);
    T(config.httpd_global_handlers._config);
    T(config.log.level);
    T(config.query_servers.javascript);
    
    // test that settings can be altered
    xhr = CouchDB.request("PUT", "/_config/test/foo",{
      body : JSON.stringify("bar")
    });
    T(xhr.status == 200);
    xhr = CouchDB.request("GET", "/_config/test");
    config = JSON.parse(xhr.responseText);
    T(config.foo == "bar");

    // you can get a single key
    xhr = CouchDB.request("GET", "/_config/test/foo");
    T(xhr.responseText == '"bar"');
  },

  security_validation : function(debug) {
    // This tests couchdb's security and validation features. This does
    // not test authentication, except to use test authentication code made
    // specifically for this testing. It is a WWWW-Authenticate scheme named
    // X-Couch-Test-Auth, and the user names and passwords are hard coded
    // on the server-side.
    // 
    // We could have used Basic authentication, however the XMLHttpRequest
    // implementation for Firefox and Safari, and probably other browsers are
    // broken (Firefox always prompts the user on 401 failures, Safari gives
    // odd security errors when using different name/passwords, perhaps due
    // to cross site scripting prevention).  These problems essentially make Basic
    // authentication testing in the browser impossible. But while hard to
    // test automated in the browser, Basic auth may still useful for real
    // world use where these bugs/behaviors don't matter.
    //
    // So for testing purposes we are using this custom X-Couch-Test-Auth.
    // It's identical to Basic auth, except it doesn't even base64 encode
    // the "username:password" string, it's sent completely plain text.
    // Firefox and Safari both deal with this correctly (which is to say
    // they correctly do nothing special).
    
    
    var db = new CouchDB("test_suite_db");
    db.deleteDb();
    db.createDb();
    if (debug) debugger;
    
    run_on_modified_server(
      [{section: "httpd",
        key: "authentication_handler",
        value: "{couch_httpd, special_test_authentication_handler}"},
      {section:"httpd",
        key: "WWW-Authenticate",
        value:  "X-Couch-Test-Auth"}],
        
      function () {
    
        // try saving document usin the wrong credentials
        var wrongPasswordDb = new CouchDB("test_suite_db",
          {"WWW-Authenticate": "X-Couch-Test-Auth Damien Katz:foo"}
        );
    
        try {
          wrongPasswordDb.save({foo:1,author:"Damien Katz"});
          T(false && "Can't get here. Should have thrown an error 1");
        } catch (e) {
          T(e.error == "unauthorized");
          T(wrongPasswordDb.last_req.status == 401);
        }
        
        
        // Create the design doc that will run custom validation code
        var designDoc = {
          _id:"_design/test",
          language: "javascript",
          validate_doc_update: "(" + (function (newDoc, oldDoc, userCtx) {
            // docs should have an author field.
            if (!newDoc._deleted && !newDoc.author) {
              throw {forbidden:
                  "Documents must have an author field"};
            }
            if (oldDoc && oldDoc.author != userCtx.name) {
                throw {unauthorized:
                    "You are not the author of this document. You jerk."};
            }
          }).toString() + ")"
        }

        // Save a document normally
        var userDb = new CouchDB("test_suite_db",
          {"WWW-Authenticate": "X-Couch-Test-Auth Damien Katz:pecan pie"}
        );
        
        T(userDb.save({_id:"testdoc", foo:1, author:"Damien Katz"}).ok);
        
        // Attempt to save the design as a non-admin
        try {
          userDb.save(designDoc);
          T(false && "Can't get here. Should have thrown an error on design doc");
        } catch (e) {
          T(e.error == "unauthorized");
          T(userDb.last_req.status == 401);
        }
        
        // add user as admin
        db.setAdmins(["Damien Katz"]);
        
        T(userDb.save(designDoc).ok);
    
        // update the document
        var doc = userDb.open("testdoc");
        doc.foo=2;
        T(userDb.save(doc).ok);
        
        // Save a document that's missing an author field.
        try {
          userDb.save({foo:1});
          T(false && "Can't get here. Should have thrown an error 2");
        } catch (e) {
          T(e.error == "forbidden");
          T(userDb.last_req.status == 403);
        }
    
        // Now attempt to update the document as a different user, Jan 
        var user2Db = new CouchDB("test_suite_db",
          {"WWW-Authenticate": "X-Couch-Test-Auth Jan Lehnardt:apple"}
        );
    
        var doc = user2Db.open("testdoc");
        doc.foo=3;
        try {
          user2Db.save(doc);
          T(false && "Can't get here. Should have thrown an error 3");
        } catch (e) {
          T(e.error == "unauthorized");
          T(user2Db.last_req.status == 401);
        }
        
        // Now have Damien change the author to Jan
        doc = userDb.open("testdoc");
        doc.author="Jan Lehnardt";
        T(userDb.save(doc).ok);
        
        // Now update the document as Jan
        doc = user2Db.open("testdoc");
        doc.foo = 3;
        T(user2Db.save(doc).ok);
        
        // Damien can't delete it
        try {
          userDb.deleteDoc(doc);
          T(false && "Can't get here. Should have thrown an error 4");
        } catch (e) {
          T(e.error == "unauthorized");
          T(userDb.last_req.status == 401);
        }
        
        // Now delete document
        T(user2Db.deleteDoc(doc).ok);
      });
  },
  
  
  max_dbs_open : function(debug) {
    if (debug) debugger;
    restartServer();
    var max = 5;
    run_on_modified_server(
      [{section: "couchdb",
        key: "max_dbs_open",
        value: max.toString()}],
        
      function () {
        for(var i=0; i<max*2; i++) {
          var db = new CouchDB("test_suite_db"+ i);
          db.deleteDb();
          db.createDb();
        }
        
        var stats = JSON.parse(CouchDB.request("GET", "/_stats").responseText);
        T(stats.dbs_open == max);
        
        
        for(var i=0; i<max*2; i++) {
          var db = new CouchDB("test_suite_db"+ i);
          db.deleteDb();
        }
        
        var stats = JSON.parse(CouchDB.request("GET", "/_stats").responseText);
        T(stats.dbs_open == 0);
      })
  },
};

function makeDocs(start, end, templateDoc) {
  var templateDocSrc = templateDoc ? JSON.stringify(templateDoc) : "{}"
  if (end === undefined) {
    end = start;
    start = 0;
  }
  var docs = []
  for (var i = start; i < end; i++) {
    var newDoc = eval("(" + templateDocSrc + ")");
    newDoc._id = (i).toString();
    newDoc.integer = i;
    newDoc.string = (i).toString();
    docs.push(newDoc)
  }
  return docs;
}

function run_on_modified_server(settings, fun) {
  try {
    // set the settings
    for(var i=0; i < settings.length; i++) {
      var s = settings[i];
      var xhr = CouchDB.request("PUT", "/_config/" + s.section + "/" + s.key, {
        body: JSON.stringify(s.value),
        headers: {"X-Couch-Persist": "false"}
      });
      CouchDB.maybeThrowError(xhr);
      s.oldValue = xhr.responseText;
    }
    // run the thing
    fun();
  } finally {
    // unset the settings
    for(var j=0; j < i; j++) {
      var s = settings[j];
      CouchDB.request("PUT", "/_config/" + s.section + "/" + s.key, {
        body: s.oldValue,
        headers: {"X-Couch-Persist": "false"}
      });
    }
  }
}

function restartServer() {
  CouchDB.request("POST", "/_restart");
}
