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

couchTests.cookie_auth = function(debug) {
  // This tests cookie-based authentication.
  
  var db = new CouchDB("test_suite_db");
  db.deleteDb();
  db.createDb();
  if (debug) debugger;

  // Simple secret key generator
  function generateSecret(length) {
    var secret = '';
    for (var i=0; i<length; i++) {
      secret += String.fromCharCode(Math.floor(Math.random() * 256));
    }
    return secret;
  }
  // this function will be called on the modified server
  var testFun = function () {
    try {
      // try using an invalid cookie
      var usersDb = new CouchDB("test_suite_users");
      usersDb.deleteDb();
      usersDb.createDb();
      
      var password = "3.141592653589";

      // Create a user
      T(usersDb.save({
        _id: "a1",
        salt: "123",
        password_sha: "8da1CtkFvb58LWrnup5chgdZVUs=",
        username: "Jason Davies",
        author: "Jason Davies",
        type: "user",
        roles: ["_admin"]
      }).ok);

      var validationDoc = {
        _id : "_design/validate",
        validate_doc_update: "(" + (function (newDoc, oldDoc, userCtx) {
          // docs should have an author field.
          if (!newDoc._deleted && !newDoc.author) {
            throw {forbidden:
                "Documents must have an author field"};
          }
          if (oldDoc && oldDoc.author != userCtx.name) {
              throw {unauthorized:
                  "You are not the author of this document. You jerk."+userCtx.name};
          }
        }).toString() + ")"
      };

      T(db.save(validationDoc).ok);

      

      T(CouchDB.login('Jason Davies', password).ok);
      // update the credentials document
      var doc = usersDb.open("a1");
      doc.foo=2;
      T(usersDb.save(doc).ok);

      // Save a document that's missing an author field.
      try {
        // db has a validation function
        db.save({foo:1});
        T(false && "Can't get here. Should have thrown an error 2");
      } catch (e) {
        T(e.error == "forbidden");
        T(db.last_req.status == 403);
      }

      // TODO should login() throw an exception here?
      T(!CouchDB.login('Jason Davies', "2.71828").ok);
      T(!CouchDB.login('Robert Allen Zimmerman', 'd00d').ok);

      // test redirect
      xhr = CouchDB.request("POST", "/_session?next=/", {
        headers: {"Content-Type": "application/x-www-form-urlencoded"},
        body: "username=Jason%20Davies&password="+encodeURIComponent(password)
      });
      // should this be a redirect code instead of 200?
      T(xhr.status == 200);

    } finally {
      // Make sure we erase any auth cookies so we don't affect other tests
      T(CouchDB.logout().ok);
    }
  };

  run_on_modified_server(
    [{section: "httpd",
      key: "authentication_handler",
      value: "{couch_httpd_auth, cookie_authentication_handler}"},
     {section: "couch_httpd_auth",
      key: "secret", value: generateSecret(64)},
     {section: "couch_httpd_auth",
      key: "authentication_db", value: "test_suite_users"}],
    testFun
  );

};
