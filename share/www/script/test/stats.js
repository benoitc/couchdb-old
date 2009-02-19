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


couchTests.stats = function(debug) {
  if (debug) debugger;

  var open_databases_tests = {
    'should increment the number of open databases when creating a db': function(name) {
      var db = new CouchDB("test_suite_db");
      db.deleteDb();
      var open_databases = parseInt(CouchDB.requestStats("couch_db", "open_databases").count);
      db.createDb();

      var new_open_databases = parseInt(CouchDB.requestStats("couch_db", "open_databases").count);
      TEquals(parseInt(open_databases) + 1, parseInt(new_open_databases), name);
    }// ,
    //       'should increment the number of open databases when opening a db': function(name) {
    //         var db = new CouchDB("test_suite_db");
    //         db.deleteDb();
    //         db.createDb();
    //         
    //         restartServer();
    // 
    //         var open_databases = parseInt(CouchDB.requestStats("couch_db", "open_databases").count);
    // 
    //         db.open("123");
    // 
    //         var new_open_databases = parseInt(CouchDB.requestStats("couch_db", "open_databases"));
    //         TEquals(parseInt(open_databases) + 1, parseInt(new_open_databases), name);
    //       },
    //         'should decrement the number of open databases when deleting': function(name) {
    //         var db = new CouchDB("test_suite_db");
    //         db.deleteDb();
    //         db.createDb();
    //         var open_databases = parseInt(CouchDB.requestStats("couch_db", "open_databases").count);
    // 
    //         db.deleteDb();
    //         var new_open_databases = parseInt(CouchDB.requestStats("couch_db", "open_databases"));
    //         TEquals(parseInt(open_databases) - 1, parseInt(new_open_databases), name);
    //       },
    //       'should keep the same number of open databases when reaching the max_dbs_open limit': function(name) {
    //         restartServer();
    //         var max = 5;
    //         run_on_modified_server(
    //           [{section: "couchdb",
    //             key: "max_dbs_open",
    //             value: max.toString()}],
    // 
    //           function () {
    //             for(var i=0; i<max+1; i++) {
    //               var db = new CouchDB("test_suite_db"+ i);
    //               db.deleteDb();
    //               db.createDb();
    //             }
    // 
    //             var open_databases = parseInt(CouchDB.requestStats("couch_db", "open_databases").count);
    //             TEquals(max, parseInt(open_databases), name);
    // 
    // 
    //             // not needed for the test but cleanup is nice
    //             for(var i=0; i<max*2; i++) {
    //               var db = new CouchDB("test_suite_db"+ i);
    //               db.deleteDb();
    //             }
    //           })
    //       },
    //       'should return 0 for number of open databases after call to restartServer()': function(name) {
    //         var db = new CouchDB("test_suite_db");
    //         db.deleteDb();
    //         db.createDb();
    // 
    //         restartServer();
    //         var open_databases = parseInt(CouchDB.requestStats("couch_db", "open_databases").count);
    //         
    //         TEquals(0, parseInt(open_databases), name);
    //       }
  };
  
  var request_count_tests = {
    'should increase the request count for every request': function(name) {
      var requests = parseInt(CouchDB.requestStats("httpd", "requests")) + 1;

      CouchDB.request("GET", "/");

      var new_requests = parseInt(CouchDB.requestStats("httpd", "requests"));
      T(requests >= 0, "requests >= 0", name);
      TEquals(requests + 1, new_requests, name);
    }
  };
  
  var document_read_count_tests = {
    'should increase read document counter when a document is read': function(name) {
      var db = new CouchDB("test_suite_db");
      db.deleteDb();
      db.createDb();
      db.save({"_id":"test"});

      var reads = parseInt(CouchDB.requestStats("httpd", "document_reads"));
      db.open("test");
      var new_reads = parseInt(CouchDB.requestStats("httpd", "document_reads"));

      TEquals(reads + 1 , new_reads, name);
    },
    'should not increase read document counter when a non-document is read': function(name) {
      var db = new CouchDB("test_suite_db");
      db.deleteDb();
      db.createDb();
      db.save({"_id":"test"});

      var reads = parseInt(CouchDB.requestStats("httpd", "document_reads"));
      CouchDB.request("GET", "/");
      var new_reads = parseInt(CouchDB.requestStats("httpd", "document_reads"));

      TEquals(reads, new_reads, name);
    },
    'should increase read document counter when a document\'s revisions are read': function(name) {
      var db = new CouchDB("test_suite_db");
      db.deleteDb();
      db.createDb();
      db.save({"_id":"test"});

      var reads = parseInt(CouchDB.requestStats("httpd", "document_reads"));
      db.open("test", {"open_revs":"all"});
      var new_reads = parseInt(CouchDB.requestStats("httpd", "document_reads"));

      TEquals(reads + 1 , new_reads, name);
    }
  };

  var view_read_count_tests = {
    'should increase the permanent view read counter': function(name) {
      var db = new CouchDB("test_suite_db");
      db.deleteDb();
      db.createDb();

      var reads = parseInt(CouchDB.requestStats("httpd", "view_reads"));
      createAndRequestView(db);
      var new_reads = parseInt(CouchDB.requestStats("httpd", "view_reads"));

      TEquals(reads + 1 , new_reads, name);
    },
    'should not increase the permanent view read counter when a document is read': function(name) {
      var db = new CouchDB("test_suite_db");
      db.deleteDb();
      db.createDb();
      db.save({"_id":"test"});

      var reads = parseInt(CouchDB.requestStats("httpd", "view_reads"));
      db.open("test");
      var new_reads = parseInt(CouchDB.requestStats("httpd", "view_reads"));

      TEquals(reads, new_reads, name);
    },
    'should not increase the permanent view read counter when a temporary view is read': function(name) {
      var db = new CouchDB("test_suite_db");
      db.deleteDb();
      db.createDb();

      var reads = parseInt(CouchDB.requestStats("httpd", "view_reads"));
      db.query(function(doc) { emit(doc._id)});
      var new_reads = parseInt(CouchDB.requestStats("httpd", "view_reads"));

      TEquals(reads, new_reads, name);
    },
    'should increase the temporary view read counter': function(name) {
      var db = new CouchDB("test_suite_db");
      db.deleteDb();
      db.createDb();

      var reads = parseInt(CouchDB.requestStats("httpd", "temporary_view_reads"));
      db.query(function(doc) { emit(doc._id)});
      var new_reads = parseInt(CouchDB.requestStats("httpd", "temporary_view_reads"));

      TEquals(reads + 1, new_reads, name);
    },
    'should increase the temporary view read counter when querying a permanent view': function(name) {
      var db = new CouchDB("test_suite_db");
      db.deleteDb();
      db.createDb();

      var reads = parseInt(CouchDB.requestStats("httpd", "view_reads"));
      createAndRequestView(db);
      var new_reads = parseInt(CouchDB.requestStats("httpd", "view_reads"));

      TEquals(reads + 1 , new_reads, name);
    }
  };
  
  var http_requests_by_method_tests = {
    'should count GET requests': function(name) {
      var requests = parseInt(CouchDB.requestStats("httpd", "get_requests"));
      var new_requests = parseInt(CouchDB.requestStats("httpd", "get_requests"));

      TEquals(requests + 1, new_requests, name);
    },
    'should not count GET requests for POST request': function(name) {
      var requests = parseInt(CouchDB.requestStats("httpd", "get_requests"));
      CouchDB.request("POST", "/");
      var new_requests = parseInt(CouchDB.requestStats("httpd", "get_requests"));

      TEquals(requests + 1, new_requests, name);        
    },
    'should count POST requests': function(name) {
      var requests = parseInt(CouchDB.requestStats("httpd", "post_requests"));
      CouchDB.request("POST", "/");
      var new_requests = parseInt(CouchDB.requestStats("httpd", "post_requests"));

      TEquals(requests + 1, new_requests, name);
    }
  };

  var document_write_count_tests = {
    'should increment counter for document creates': function(name) {
      var db = new CouchDB("test_suite_db");
      db.deleteDb();
      db.createDb();

      var creates = parseInt(CouchDB.requestStats("httpd", "document_creates"));
      db.save({"a":"1"});
      var new_creates = parseInt(CouchDB.requestStats("httpd", "document_creates"));

      TEquals(creates + 1, new_creates, name);
    },
    'should not increment counter for document creates when updating a doc': function(name) {
      var db = new CouchDB("test_suite_db");
      db.deleteDb();
      db.createDb();

      var doc = {"_id":"test"};
      db.save(doc);
      
      var creates = parseInt(CouchDB.requestStats("httpd", "document_creates"));
      db.save(doc);
      var new_creates = parseInt(CouchDB.requestStats("httpd", "document_creates"));

      TEquals(creates, new_creates, name);
    },
    'should increment counter for document updates': function(name) {
      var db = new CouchDB("test_suite_db");
      db.deleteDb();
      db.createDb();

      var doc = {"_id":"test"};
      db.save(doc);
      
      var updates = parseInt(CouchDB.requestStats("httpd", "document_updates"));
      db.save(doc);
      var new_updates = parseInt(CouchDB.requestStats("httpd", "document_updates"));

      TEquals(updates + 1, new_updates, name);
    },
    'should not increment counter for document updates when creating a document': function(name) {
      var db = new CouchDB("test_suite_db");
      db.deleteDb();
      db.createDb();

      var updates = parseInt(CouchDB.requestStats("httpd", "document_updates"));
      db.save({"a":"1"});
      var new_updates = parseInt(CouchDB.requestStats("httpd", "document_updates"));

      TEquals(updates, new_updates, name);
    },
    'should increment counter for document deletes': function(name) {
      var db = new CouchDB("test_suite_db");
      db.deleteDb();
      db.createDb();

      var doc = {"_id":"test"};
      db.save(doc);
      
      var deletes = parseInt(CouchDB.requestStats("httpd", "document_deletes"));
      db.deleteDoc(doc);
      var new_deletes = parseInt(CouchDB.requestStats("httpd", "document_deletes"));

      TEquals(deletes + 1, new_deletes, name);
    },
    'should increment the copy counter': function(name) {
      var db = new CouchDB("test_suite_db");
      db.deleteDb();
      db.createDb();

      var doc = {"_id":"test"};
      db.save(doc);

      var copies = parseInt(CouchDB.requestStats("httpd", "document_copies"));
      CouchDB.request("COPY", "/test_suite_db/test", {
        headers: {"Destination":"copy_of_test"}
      });
      var new_copies = parseInt(CouchDB.requestStats("httpd", "document_copies"));

      TEquals(copies + 1, new_copies, name);
    },
    'should increment the move counter': function(name) {
      var db = new CouchDB("test_suite_db");
      db.deleteDb();
      db.createDb();

      var doc = {"_id":"test"};
      db.save(doc);

      var moves = parseInt(CouchDB.requestStats("httpd", "document_moves"));
      CouchDB.request("MOVE", "/test_suite_db/test?rev=" + doc._rev, {
        headers: {"Destination":"move_of_test"}
      });
      var new_moves = parseInt(CouchDB.requestStats("httpd", "document_moves"));

      TEquals(moves + 1, new_moves, name);
    },
    'should increase the bulk doc counter': function(name) {
      var db = new CouchDB("test_suite_db");
      db.deleteDb();
      db.createDb();

      var bulks = parseInt(CouchDB.requestStats("httpd", "bulk_requests"));

      var docs = makeDocs(5);
      db.bulkSave(docs);
      
      var new_bulks = parseInt(CouchDB.requestStats("httpd", "bulk_requests"));

      TEquals(bulks + 1, new_bulks, name);
    },
    'should increment counter for document creates using POST': function(name) {
      var db = new CouchDB("test_suite_db");
      db.deleteDb();
      db.createDb();

      var creates = parseInt(CouchDB.requestStats("httpd", "document_creates"));
      CouchDB.request("POST", "/test_suite_db", {body:'{"a":"1"}'});
      var new_creates = parseInt(CouchDB.requestStats("httpd", "document_creates"));

      TEquals(creates + 1, new_creates, name);
    },
    'should increment document create counter when adding attachment': function(name) {
      var db = new CouchDB("test_suite_db");
      db.deleteDb();
      db.createDb();

      var creates = parseInt(CouchDB.requestStats("httpd", "document_creates"));
      CouchDB.request("PUT", "/test_suite_db/bin_doc2/foo2.txt", {
            body:"This is no base64 encoded text",
            headers:{"Content-Type": "text/plain;charset=utf-8"}
      });
      var new_creates = parseInt(CouchDB.requestStats("httpd", "document_creates"));
      TEquals(creates + 1, new_creates, name);
    },
    'should increment document update counter when adding attachment to existing doc': function(name) {
      var db = new CouchDB("test_suite_db");
      db.deleteDb();
      db.createDb();

      var doc = {_id:"test"};
      db.save(doc);

      var updates = parseInt(CouchDB.requestStats("httpd", "document_updates"));
      CouchDB.request("PUT", "/test_suite_db/test/foo2.txt?rev=" + doc._rev, {
            body:"This is no base64 encoded text",
            headers:{"Content-Type": "text/plain;charset=utf-8"}
      });
      var new_updates = parseInt(CouchDB.requestStats("httpd", "document_updates"));
      TEquals(updates + 1, new_updates, name);
    }

  };
  var response_codes_tests = {
    'should increment the response code counter': function(name) {
      var db = new CouchDB("nonexistant_db");
      db.deleteDb();

      var not_founds = parseInt(CouchDB.requestStats("http_status_codes", "404"));
      CouchDB.request("GET", "/nonexistant_db");
      var new_not_founds = parseInt(CouchDB.requestStats("http_status_codes", "404"));

      TEquals(not_founds + 1, new_not_founds, name);
    },
    'should not increment respinse code counter for other response code': function(name) {
      var not_founds = parseInt(CouchDB.requestStats("http_status_codes", "404"));
      CouchDB.request("GET", "/");
      var new_not_founds = parseInt(CouchDB.requestStats("http_status_codes", "404"));

      TEquals(not_founds, new_not_founds, name);
    }
  };

  var aggregation_tests = {
    'should return the mean': function(name) {
      CouchDB.request("GET", "/");

      var mean = parseInt(CouchDB.requestStats("httpd", "requests", "mean"));

      T(mean >= 0, name);
    },
    'should return the maximum': function(name) {
      CouchDB.request("GET", "/");

      var maximum = parseInt(CouchDB.requestStats("httpd", "requests","max"));

      T(maximum >= 0, name);
    },
    'should return the minimum': function(name) {
      CouchDB.request("GET", "/");

      var minimum = parseInt(CouchDB.requestStats("httpd", "requests", "min"));

      T(minimum >= 0, name);
    },
    'should return the stddev': function(name) {
      CouchDB.request("GET", "/");

      var stddev = parseInt(CouchDB.requestStats("httpd", "stddev_requests"));

      T(stddev >= 0, name);
    }
  };

  var summary_tests = {
    'should show a summary of all counters with aggregated values': function(name) {
      var options = {};
      options.headers = {"Accept": "application/json"};
      var summary = JSON.parse(CouchDB.request("GET", "/_stats", options).responseText);
      var aggregates = ["mean", "min", "max", "stddev", 
        "current", "resolution"];

      for(var i in aggregates) {
        T(summary.httpd.requests[aggregates[i]] >= 0, aggregates[i] + " >= 0", name);
      }
    }
  };

  var tests = [
    open_databases_tests, 
    request_count_tests, 
    document_read_count_tests, 
    view_read_count_tests, 
    http_requests_by_method_tests,
    document_write_count_tests,
    response_codes_tests,
    aggregation_tests,
    summary_tests
  ];
  
  var tests = [open_databases_tests];

  for(var testGroup in tests) {
    for(var test in tests[testGroup]) {
      tests[testGroup][test](test);
    }
  };

  function createAndRequestView(db) {
    var designDoc = {
      _id:"_design/test", // turn off couch.js id escaping?
      language: "javascript",
      views: {
        all_docs_twice: {map: "function(doc) { emit(doc.integer, null); emit(doc.integer, null) }"},
      }
    };
    db.save(designDoc);

    db.view("test/all_docs_twice");
  }

}
