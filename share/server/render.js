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

// this function provides a shortcut for managing responses by Accept header
respondWith = function(req, responders) {
  var bestKey = null, accept = req.headers["Accept"];
  if (accept && !req.query.format) {
    var provides = [];
    for (key in responders) {
      if (mimesByKey[key]) {
        provides = provides.concat(mimesByKey[key]);        
      }
    }
    var bestMime = Mimeparse.bestMatch(provides, accept);
    bestKey = keysByMime[bestMime];
  } else {
    bestKey = req.query.format;
  }
  var rFunc = responders[bestKey || responders.fallback || "html"];
  if (rFunc) {      
    var resp = rFunc();
    resp["headers"] = resp["headers"] || {};
    resp["headers"]["Content-Type"] = bestMime;
    respond(resp);
  } else {
    throw({code:406, body:"Not Acceptable: "+accept});    
  }
};

// whoever registers last wins.
mimesByKey = {};
keysByMime = {};
registerType = function() {
  var mimes = [], key = arguments[0];
  for (var i=1; i < arguments.length; i++) {
    mimes.push(arguments[i]);
  };
  mimesByKey[key] = mimes;
  for (var i=0; i < mimes.length; i++) {
    keysByMime[mimes[i]] = key;
  };
};

// Some default types
// Ported from Ruby on Rails
// Build list of Mime types for HTTP responses
// http://www.iana.org/assignments/media-types/
// http://dev.rubyonrails.org/svn/rails/trunk/actionpack/lib/action_controller/mime_types.rb

registerType("all", "*/*");
registerType("text", "text/plain", "txt");
registerType("html", "text/html");
registerType("xhtml", "application/xhtml+xml", "xhtml");
registerType("xml", "application/xml", "text/xml", "application/x-xml");
registerType("js", "text/javascript", "application/javascript", "application/x-javascript");
registerType("css", "text/css");
registerType("ics", "text/calendar");
registerType("csv", "text/csv");
registerType("rss", "application/rss+xml");
registerType("atom", "application/atom+xml");
registerType("yaml", "application/x-yaml", "text/yaml");

// just like Rails
registerType("multipart_form", "multipart/form-data");
registerType("url_encoded_form", "application/x-www-form-urlencoded");

// http://www.ietf.org/rfc/rfc4627.txt
registerType("json", "application/json", "text/x-json");



//  Send chunk
function sendHeaders(headers) {
  respond(["headers", headers]);
}

function sendChunk(chunk) {
  respond(["chunk", chunk]);
};

function getRow() {
  var line = readline();
  var json = eval(line);
  if (json[0] == "list_end") return null;
  if (json[0] != "list_row") {
    respond({
      error: "query_server_error",
      reason: "not a row '" + json[0] + "'"});
    quit();
  }
  return json[1];
};

////
////  Render dispatcher
////
////
////
////

var Render = (function() {
  var row_info;
  
  return {
    show : function(funSrc, doc, req) {
      var formFun = compileFunction(funSrc);
      runRenderFunction(formFun, [doc, req], funSrc, true);
    },
    list : function(head, req) {
      // log("run list yo");
      // var oldLog = plog;
      // plog = function(){};
      runRenderFunction(funs[0], [head, req], funsrc[0]);
      // plog = oldLog;
    }
  }
})();

function runRenderFunction(renderFun, args, funSrc, htmlErrors) {
  try {
    var resp = renderFun.apply(null, args);
      if (resp) {
        respond(["end", resp]);
      } else {
        respond({error:"render_error",reason:"undefined response from render function"});
      }      
  } catch(e) {
    var logMessage = "function raised error: "+e.toString();
    // log(logMessage);
    // log("stacktrace: "+e.stack);
    var errorMessage = htmlErrors ? htmlRenderError(e, funSrc) : logMessage;
    
    respond({
      error:"render_error",
      reason:errorMessage});
  }
};

function escapeHTML(string) {
  return string.replace(/&/g, "&amp;")
               .replace(/</g, "&lt;")
               .replace(/>/g, "&gt;");
}

function htmlRenderError(e, funSrc) {
  var msg = ["<html><body><h1>Render Error</h1>",
    "<p>JavaScript function raised error: ",
    e.toString(),
    "</p><h2>Stacktrace:</h2><code><pre>",
    escapeHTML(e.stack),
    "</pre></code><h2>Function source:</h2><code><pre>",
    escapeHTML(funSrc),
    "</pre></code></body></html>"].join('');
  return {body:msg};
};

