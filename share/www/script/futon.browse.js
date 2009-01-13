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

(function($) {
  $.futon = $.futon || {};
  $.extend($.futon, {

    // Page class for browse/index.html
    CouchIndexPage: function() {
      page = this;

      this.addDatabase = function() {
        $.showDialog("dialog/_create_database.html", {
          submit: function(data, callback) {
            if (!data.name || data.name.length == 0) {
              callback({name: "Please enter a name."});
              return;
            }
            $.couch.db(data.name).create({
              error: function(status, id, reason) { callback({name: reason}) },
              success: function(resp) {
                location.href = "database.html?" + encodeURIComponent(data.name);
                callback();
              }
            });
          }
        });
        return false;
      }

      this.updateDatabaseListing = function(offset) {
        offset |= 0;
        $(document.body).addClass("loading");
        var maxPerPage = parseInt($("#perpage").val(), 10);

        $.couch.allDbs({
          success: function(dbs) {
            $("#paging a").unbind();
            $("#databases tbody.content").empty();

            if (dbs.length == 0) {
              $(document.body).removeClass("loading");
            }
            var dbsOnPage = dbs.slice(offset, offset + maxPerPage);

            $.each(dbsOnPage, function(idx, dbName) {
              $("#databases tbody.content").append("<tr>" + 
                "<th><a href='database.html?" + encodeURIComponent(dbName) + "'>" +
                  dbName + "</a></th>" +
                "<td class='size'></td><td class='count'></td>" +
                "<td class='seq'></td></tr>");
              $.couch.db(dbName).info({
                success: function(info) {
                  $("#databases tbody.content tr:eq(" + idx + ")")
                    .find("td.size").text($.futon.formatSize(info.disk_size)).end()
                    .find("td.count").text(info.doc_count).end()
                    .find("td.seq").text(info.update_seq);
                  if (idx == dbsOnPage.length - 1) {
                    $(document.body).removeClass("loading");
                  }
                }
              });
            });
            $("#databases tbody tr:odd").addClass("odd");

            if (offset > 0) {
              $("#paging a.prev").attr("href", "#" + (offset - maxPerPage)).click(function() {
                page.updateDatabaseListing(offset - maxPerPage);
              });
            } else {
              $("#paging a.prev").removeAttr("href");
            }
            if (offset + maxPerPage < dbs.length) {
              $("#paging a.next").attr("href", "#" + (offset + maxPerPage)).click(function() {
                page.updateDatabaseListing(offset + maxPerPage);
              });
            } else {
              $("#paging a.next").removeAttr("href");
            }

            var firstNum = offset + 1;
            var lastNum = firstNum + dbsOnPage.length - 1;
            $("#databases tbody.footer tr td span").text(
              "Showing " + firstNum + "-" + lastNum + " of " + dbs.length +
              " databases");
          }
        });
      }

    },

    // Page class for browse/database.html
    CouchDatabasePage: function() {
      var urlParts = location.search.substr(1).split("/");
      var dbName = decodeURIComponent(urlParts.shift());
      var viewName = (urlParts.length > 0) ? urlParts.join("/") : null;
      if (viewName) {
        viewName = decodeURIComponent(viewName);
        $.cookies.set(dbName + ".view", viewName);
      } else {
        viewName = $.cookies.get(dbName + ".view", "");
        if (viewName) {
          location.href = "database.html?" + dbName + "/" + viewName;
        }
      }
      var db = $.couch.db(dbName);

      this.dbName = dbName;
      this.viewName = viewName;
      this.db = db;
      this.isDirty = false;
      this.isTempView = viewName == "_slow_view";
      page = this;

      this.addDocument = function() {
        $.showDialog("dialog/_create_document.html", {
          submit: function(data, callback) {
            db.saveDoc(data.docid ? {_id: data.docid} : {}, {
              error: function(status, error, reason) {
                callback({docid: reason});
              },
              success: function(resp) {
                location.href = "document.html?" + encodeURIComponent(dbName) +
                                "/" + encodeDocId(resp.id);
              }
            });
          }
        });
      }

      this.compactDatabase = function() {
        $.showDialog("dialog/_compact_database.html", {
          submit: function(data, callback) {
            db.compact({
              success: function(resp) {
                callback();
              }
            });
          }
        });
      }

      this.deleteDatabase = function() {
        $.showDialog("dialog/_delete_database.html", {
          submit: function(data, callback) {
            db.drop({
              success: function(resp) {
                callback();
                location.href = "index.html";
                if (window !== null) {
                  $("#dbs li").filter(function(index) {
                    return $("a", this).text() == dbName;
                  }).remove();
                  $.futon.navigation.removeDatabase(dbName);
                }
              }
            });
          }
        });
      }

      this.populateViewEditor = function() {
        if (viewName.match(/^_design\//)) {
          page.revertViewChanges(function() {
            var dirtyTimeout = null;
            function updateDirtyState() {
              clearTimeout(dirtyTimeout);
              dirtyTimeout = setTimeout(function() {
                var buttons = $("#viewcode button.save, #viewcode button.revert");
                page.isDirty = ($("#viewcode_map").val() != page.storedViewCode.map)
                  || ($("#viewcode_reduce").val() != (page.storedViewCode.reduce || ""));
                if (page.isDirty) {
                  buttons.removeAttr("disabled");
                } else {
                  buttons.attr("disabled", "disabled");
                }
              }, 100);
            }
            $("#viewcode textarea").bind("input", updateDirtyState);
            if ($.browser.msie || $.browser.safari) {
              $("#viewcode textarea").bind("paste", updateDirtyState)
                                     .bind("change", updateDirtyState)
                                     .bind("keydown", updateDirtyState)
                                     .bind("keypress", updateDirtyState)
                                     .bind("keyup", updateDirtyState)
                                     .bind("textInput", updateDirtyState);
            }
          });
        } else if (viewName == "_slow_view") {
          page.updateViewEditor(
            $.cookies.get(db.name + ".map"),
            $.cookies.get(db.name + ".reduce", "")
          );
        }
      }

      this.populateViewsMenu = function() {
        var select = $("#switch select");
        db.allDocs({startkey: "_design/", endkey: "_design0",
          success: function(resp) {
            select[0].options.length = 3;
            for (var i = 0; i < resp.rows.length; i++) {
              db.openDoc(resp.rows[i].id, {
                success: function(doc) {
                  var optGroup = $("<optgroup></optgroup>").attr("label", doc._id.substr(8));
                  var optGroup = $(document.createElement("optgroup"))
                    .attr("label", doc._id.substr(8));
                  for (var name in doc.views) {
                    if (!doc.views.hasOwnProperty(name)) continue;
                    var option = $(document.createElement("option"))
                      .attr("value", doc._id + "/" + name).text(name)
                      .appendTo(optGroup);
                    if (doc._id + "/" + name == viewName) {
                      option[0].selected = true;
                    }
                  }
                  optGroup.appendTo(select);
                }
              });
            }
          }
        });
        if (!viewName.match(/^_design\//)) {
          $.each(["_all_docs", "_design_docs", "_slow_view"], function(idx, name) {
            if (viewName == name) {
              select[0].options[idx].selected = true;
            }
          });
        }
      }

      this.revertViewChanges = function(callback) {
        if (!page.storedViewCode) {
          var viewNameParts = viewName.split("/");
          var designDocId = viewNameParts[1];
          var localViewName = viewNameParts[2];
          db.openDoc(["_design", designDocId].join("/"), {
            error: function(status, error, reason) {
              if (status == 404) {
                $.cookies.remove(dbName + ".view");
                location.reload();
              }
            },
            success: function(resp) {
              var viewCode = resp.views[localViewName];
              page.updateViewEditor(viewCode.map, viewCode.reduce || "");
              $("#viewcode button.revert, #viewcode button.save").attr("disabled", "disabled");
              page.storedViewCode = viewCode;
              if (callback) callback();
            }
          });
        } else {
          $("#viewcode_map").val(page.storedViewCode.map);
          $("#viewcode_reduce").val(page.storedViewCode.reduce || "");
          page.isDirty = false;
          $("#viewcode button.revert, #viewcode button.save").attr("disabled", "disabled");
          if (callback) callback();
        }
      }

      this.updateViewEditor = function(mapFun, reduceFun) {
        if (!mapFun) return;
        $("#viewcode_map").val(mapFun);
        $("#viewcode_reduce").val(reduceFun);
        var lines = Math.max(
          mapFun.split("\n").length,
          reduceFun.split("\n").length
        );
        $("#viewcode textarea").attr("rows", Math.min(15, Math.max(3, lines)));
      }

      this.saveViewAs = function() {
        if (viewName && /^_design/.test(viewName)) {
          var viewNameParts = viewName.split("/");
          var designDocId = viewNameParts[1];
          var localViewName = viewNameParts[2];
        } else {
          var designDocId = "", localViewName = "";
        }
        $.showDialog("dialog/_save_view_as.html", {
          load: function(elem) {
            $("#input_docid", elem).val(designDocId).suggest(function(text, callback) {
              db.allDocs({
                limit: 10, startkey: "_design/" + text,
                endkey: "_design/" + text + "ZZZZ",
                success: function(docs) {
                  var matches = [];
                  for (var i = 0; i < docs.rows.length; i++) {
                    matches[i] = docs.rows[i].id.substr(8);
                  }
                  callback(matches);
                }
              });
            });
            $("#input_name", elem).val(localViewName).suggest(function(text, callback) {
              db.openDoc("_design/" + $("#input_docid").val(), {
                error: function() {}, // ignore
                success: function(doc) {
                  var matches = [];
                  if (!doc.views) return;
                  for (var viewName in doc.views) {
                    if (!doc.views.hasOwnProperty(viewName) || !viewName.match("^" + text)) {
                      continue;
                    }
                    matches.push(viewName);
                  }
                  callback(matches);
                }
              });
            });
          },
          submit: function(data, callback) {
            if (!data.docid || !data.name) {
              var errors = {};
              if (!data.docid) errors.docid = "Please enter a document ID";
              if (!data.name) errors.name = "Please enter a view name";
              callback(errors);
            } else {
              var viewCode = {
                map: $("#viewcode_map").val(),
                reduce: $("#viewcode_reduce").val() || undefined
              };
              var docId = ["_design", data.docid].join("/");
              function save(doc) {
                if (!doc) doc = {_id: docId, language: "javascript"};
                if (doc.views === undefined) doc.views = {};
                doc.views[data.name] = viewCode;
                db.saveDoc(doc, {
                  success: function(resp) {
                    callback();
                    page.isDirty = false;
                    location.href = "database.html?" + encodeURIComponent(dbName) +
                      "/" + encodeDocId(doc._id) +
                      "/" + encodeURIComponent(data.name);
                  }
                });
              }
              db.openDoc(docId, {
                error: function(status, error, reason) {
                  if (status == 404) save(null);
                  else alert(reason);
                },
                success: function(doc) {
                  save(doc);
                }
              });
            }
          }
        });
      }

      this.saveViewChanges = function() {
        var viewNameParts = viewName.split("/");
        var designDocId = viewNameParts[1];
        var localViewName = viewNameParts[2];
        $(document.body).addClass("loading");
        db.openDoc(["_design", designDocId].join("/"), {
          success: function(doc) {
            var viewDef = doc.views[localViewName];
            viewDef.map = $("#viewcode_map").val();
            viewDef.reduce = $("#viewcode_reduce").val() || undefined;
            db.saveDoc(doc, {
              success: function(resp) {
                page.isDirty = false;
                $("#viewcode button.revert, #viewcode button.save")
                  .attr("disabled", "disabled");
                $(document.body).removeClass("loading");
              }
            });
          }
        });
      }

      this.updateDesignDocLink = function() {
        if (viewName && /^_design/.test(viewName)) {
          var docId = "_design/" + viewName.split("/")[1];
          $("#designdoc-link").attr("href", "document.html?" +
            encodeURIComponent(dbName) + "/" + encodeDocId(docId)).text(docId);
        } else {
          $("#designdoc-link").removeAttr("href").text("");
        }
      }

      this.updateDocumentListing = function(options) {
        $(document.body).addClass("loading");
        if (options === undefined) options = {};
        if (options.limit === undefined) {
          options.limit = parseInt($("#perpage").val(), 10);
        }
        if (options.group === undefined) {
          options.group = true;
        }
        if ($("#documents thead th.key").is(".desc")) {
          if (typeof options.descending == 'undefined') options.descending = true;
          var descend = true;
          $.cookies.set(dbName + ".desc", "1");
        } else {
          var descend = false;
          $.cookies.remove(dbName + ".desc");
        }
        $("#paging a").unbind();
        $("#documents").find("tbody.content").empty().end().show();
        this.updateDesignDocLink();

        options.success = function(resp) {
          if (resp.offset === undefined) {
            resp.offset = 0;
          }
          var decending_reverse = ((options.descending && !descend) || (descend && (options.descending === false)));
          if (decending_reverse && resp.rows) {
            resp.rows = resp.rows.reverse();
          }
          if (resp.rows !== null && (decending_reverse ? 
            (resp.total_rows - resp.offset > options.limit) :
            (resp.offset > 0))) {
            $("#paging a.prev").attr("href", "#" + (resp.offset - options.limit)).click(function() {
              var firstDoc = resp.rows[0];
              page.updateDocumentListing({
                startkey: firstDoc.key !== undefined ? firstDoc.key : null,
                startkey_docid: firstDoc.id,
                skip: 1,
                descending: !descend,
                limit: options.limit
              });
              return false;
            });
          } else {
            $("#paging a.prev").removeAttr("href");
          }
          if (resp.rows !== null && (decending_reverse ? 
            (resp.offset - resp.total_rows < options.limit) : 
            (resp.total_rows - resp.offset > options.limit))) {
            $("#paging a.next").attr("href", "#" + (resp.offset + options.limit)).click(function() {
              var lastDoc = resp.rows[resp.rows.length - 1];
              page.updateDocumentListing({
                startkey: lastDoc.key !== undefined ? lastDoc.key : null,
                startkey_docid: lastDoc.id,
                skip: 1,
                descending: descend,
                limit: options.limit
              });
              return false;
            });
          } else {
            $("#paging a.next").removeAttr("href");
          }

          for (var i = 0; i < resp.rows.length; i++) {
            var row = resp.rows[i];
            var tr = $("<tr></tr>");
            var key = "null";
            if (row.key !== null) {
              key = $.futon.formatJSON(row.key, {indent: 0, linesep: ""});
            }
            if (row.id) {
              $("<td class='key'><a href='document.html?" + encodeURIComponent(db.name) +
                "/" + encodeDocId(row.id) + "'><strong></strong><br>" +
                "<span class='docid'>ID:&nbsp;" + row.id + "</span></a></td>")
                .find("strong").text(key).end()
                .appendTo(tr);
            } else {
              $("<td class='key'><strong></strong></td>")
                .find("strong").text(key).end()
                .appendTo(tr);
            }
            var value = "null";
            if (row.value !== null) {
              value = $.futon.formatJSON(row.value, {
                html: true, indent: 0, linesep: "", quoteKeys: false
              });
            }
            $("<td class='value'><div></div></td>").find("div").html(value).end().appendTo(tr).dblclick(function() {
              location.href = this.previousSibling.firstChild.href;
            });
            tr.appendTo("#documents tbody.content");
          }
          var firstNum = 1;
          var lastNum = totalNum = resp.rows.length;
          if (resp.total_rows != null) {
            if (decending_reverse) {
              lastNum = Math.min(resp.total_rows, resp.total_rows - resp.offset);
              firstNum = lastNum - resp.rows.length + 1;          
            } else {
              firstNum = Math.min(resp.total_rows, resp.offset + 1);
              lastNum = firstNum + resp.rows.length - 1;
            }
            totalNum = resp.total_rows;
            $("#paging").show();
          } else {
            $("#paging").hide();
          }
          $("#documents tbody.footer td span").text(
            "Showing " + firstNum + "-" + lastNum + " of " + totalNum +
            " row" + (firstNum != lastNum ? "s" : ""));
          $("#documents tbody tr:odd").addClass("odd");
          $(document.body).removeClass("loading");
        }
        options.error = function(status, error, reason) {
          alert("Error: " + error + "\n\n" + reason);
          $(document.body).removeClass("loading");
        }

        if (!viewName || viewName == "_all_docs") {
          $("#switch select")[0].selectedIndex = 0;
          db.allDocs(options);
        } else {
          if (viewName == "_slow_view") {
            $("#viewcode").show().removeClass("collapsed");
            var mapFun = $("#viewcode_map").val();
            $.cookies.set(db.name + ".map", mapFun);
            var reduceFun = $("#viewcode_reduce").val() || null;
            if (reduceFun != null) {
              $.cookies.set(db.name + ".reduce", reduceFun);
            } else {
              $.cookies.remove(db.name + ".reduce");
            }
            db.query(mapFun, reduceFun, null, options);
          } else if (viewName == "_design_docs") {
            options.startkey = options.descending ? "_design0" : "_design";
            options.endkey = options.descending ? "_design" : "_design0";
            db.allDocs(options);
          } else {
            $("#viewcode").show();
            var currentMapCode = $("#viewcode_map").val();
            var currentReduceCode = $("#viewcode_reduce").val() || null;
            if (page.isDirty) {
              db.query(currentMapCode, currentReduceCode, null, options);
            } else {
              db.view(viewName.substr(8), options);
            }
          }
        }
      }

      window.onbeforeunload = function() {
        $("#switch select").val(viewName);
        if (page.isDirty) {
          return "You've made changes to the view code that have not been " +
                 "saved yet.";
        }
      }

    },

    // Page class for browse/database.html
    CouchDocumentPage: function() {
      var urlParts = location.search.substr(1).split("/");
      var dbName = decodeURIComponent(urlParts.shift());
      var idParts = urlParts.join("/").split("@", 2);
      var docId = decodeURIComponent(idParts[0]);
      var docRev = (idParts.length > 1) ? idParts[1] : null;
      var db = $.couch.db(dbName);

      this.dbName = dbName;
      this.db = db;
      this.docId = docId;
      this.doc = null;
      this.isDirty = false;
      page = this;

      this.activateTabularView = function() {
        $("#tabs li").removeClass("active").filter(".tabular").addClass("active");
        $("#fields thead th:first").text("Field").attr("colspan", 1).next().show();
        $("#fields tbody.content").show();
        $("#fields tbody.source").hide();
      }

      this.activateSourceView = function() {
        $("#tabs li").removeClass("active").filter(".source").addClass("active");
        $("#fields thead th:first").text("Source").attr("colspan", 2).next().hide();
        $("#fields tbody.content").hide();
        $("#fields tbody.source").find("td").each(function() {
          $(this).html($("<pre></pre>").html($.futon.formatJSON(page.doc, {html: true})));
        }).end().show();
      }

      this.addField = function() {
        if (!$("#fields tbody.content:visible").length) {
          location.hash = "#tabular";
          page.activateTabularView();
        }
        var fieldName = "unnamed";
        var fieldIdx = 1;
        while (page.doc.hasOwnProperty(fieldName)) {
          fieldName = "unnamed " + fieldIdx++;
        }
        page.doc[fieldName] = null;
        var row = _addRowForField(page.doc, fieldName);
        page.isDirty = true;
        _editKey(page.doc, row.find("th"), fieldName);
      }

      var _sortFields = function(a, b) {
        var a0 = a.charAt(0), b0 = b.charAt(0);
        if (a0 == "_" && b0 != "_") {
          return -1;
        } else if (a0 != "_" && b0 == "_") {
          return 1;
        } else if (a == "_attachments" || b == "_attachments") {
          return a0 == "_attachments" ? 1 : -1;
        } else {
          return a < b ? -1 : a != b ? 1 : 0;
        }
      }

      this.updateFieldListing = function() {
        $(document.body).addClass("loading");
        $("#fields tbody.content").empty();

        function handleResult(doc, revs) {
          page.doc = doc;
          var propNames = [];
          for (var prop in doc) {
            if (!doc.hasOwnProperty(prop)) continue;
            propNames.push(prop);
          }
          // Order properties alphabetically, but put internal fields first
          propNames.sort(_sortFields);
          for (var pi = 0; pi < propNames.length; pi++) {
            _addRowForField(doc, propNames[pi]);
          }
          if (revs.length > 1) {
            var currentIndex = 0;
            for (var i = 0; i < revs.length; i++) {
              if (revs[i].rev == doc._rev) {
                currentIndex = i;
                break;
              }
            }
            if (currentIndex < revs.length - 1) {
              var prevRev = revs[currentIndex + 1].rev;
              $("#paging a.prev").attr("href", "?" + encodeURIComponent(dbName) +
                "/" + encodeDocId(docId) + "@" + prevRev);
            }
            if (currentIndex > 0) {
              var nextRev = revs[currentIndex - 1].rev;
              $("#paging a.next").attr("href", "?" + encodeURIComponent(dbName) +
                "/" + encodeDocId(docId) + "@" + nextRev);
            }
            $("#fields tbody.footer td span").text("Showing revision " +
              (revs.length - currentIndex) + " of " + revs.length);
          }
          if (location.hash == "#source") {
            page.activateSourceView();
          }
          $(document.body).removeClass("loading");
        }

        db.openDoc(docId, {revs_info: true,
          success: function(doc) {
            var revs = doc._revs_info;
            delete doc._revs_info;
            if (docRev != null) {
              db.openDoc(docId, {rev: docRev,
                error: function(status, error, reason) {
                  alert("The requested revision was not found. " +
                        "You will be redirected back to the latest revision.");
                  location.href = "?" + encodeURIComponent(dbName) +
                    "/" + encodeDocId(docId);
                },
                success: function(doc) {
                  handleResult(doc, revs);
                }
              });
            } else {
              handleResult(doc, revs);
            }
          }
        });
      }

      this.deleteDocument = function() {
        $.showDialog("dialog/_delete_document.html", {
          submit: function(data, callback) {
            db.removeDoc(page.doc, {
              success: function(resp) {
                callback();
                location.href = "database.html?" + encodeURIComponent(dbName);
              }
            });
          }
        });
      }

      this.saveDocument = function() {
        $(document.body).addClass("loading");
        db.saveDoc(page.doc, {
          success: function(resp) {
            page.isDirty = false;
            location.href = "?" + encodeURIComponent(dbName) +
              "/" + encodeDocId(docId);
          }
        });
      }

      this.uploadAttachment = function() {
        if (page.isDirty) {
          alert("You need to save or revert any changes you have made to the " +
                "document before you can attach a new file.");
          return false;
        }
        $.showDialog("dialog/_upload_attachment.html", {
          load: function(elem) {
            $("input[name='_rev']", elem).val(page.doc._rev);
          },
          submit: function(data, callback) {
            if (!data._attachments || data._attachments.length == 0) {
              callback({_attachments: "Please select a file to upload."});
              return;
            }
            var form = $("#upload-form");
            form.find("#progress").css("visibility", "visible");
            form.ajaxSubmit({
              url: db.uri + encodeDocId(page.docId),
              success: function(resp) {
                form.find("#progress").css("visibility", "hidden");
                page.isDirty = false;
                location.href = "?" + encodeURIComponent(dbName) +
                  "/" + encodeDocId(docId);
              }
            });
          }
        });
      }

      window.onbeforeunload = function() {
        if (page.isDirty) {
          return "You've made changes to this document that have not been " +
                 "saved yet.";
        }
      }

      function _addRowForField(doc, fieldName) {
        var row = $("<tr><th></th><td></td></tr>").find("th").append($("<b></b>")
          .text(fieldName)).end().appendTo("#fields tbody.content");
        if (fieldName == "_attachments") {
          row
            .find("td").append(_renderAttachmentList(doc[fieldName]));
        } else {
          var value = _renderValue(doc[fieldName]);
          row
            .find("th b").dblclick(function() {
              _editKey(doc, this, $(this).text());
            }).end()
            .find("td").append(value).dblclick(function() {
              _editValue(doc, this, $(this).prev("th").text());
            }).end();
          if (fieldName != "_id" && fieldName != "_rev") {
            row.find("th, td").attr("title", "Double click to edit");
            _initKey(doc, row, fieldName);
            _initValue(value);
          }
        }
        $("#fields tbody.content tr").removeClass("odd").filter(":odd").addClass("odd");
        return row;
      }

      function _editKey(doc, cell, fieldName) {
        if (fieldName == "_id" || fieldName == "_rev") return;
        var th = $(cell);
        th.empty();
        var input = $("<input type='text' spellcheck='false'>");
        input.dblclick(function() { return false; }).keydown(function(evt) {
          switch (evt.keyCode) {
            case 13: applyChange(); break;
            case 27: cancelChange(); break;
          }
        });
        var tools = $("<div class='tools'></div>");
        function applyChange() {
          input.nextAll().remove();
          var newName = input.val();
          if (!newName.length || newName == fieldName) {
            cancelChange();
            return;
          }
          doc[newName] = doc[fieldName];
          delete doc[fieldName];
          th.children().remove();
          th.append($("<b></b>").text(newName));
          _initKey(doc, th.parent("tr"), newName);
          page.isDirty = true;
        }
        function cancelChange() {
          th.children().remove();
          th.append($("<b></b>").text(fieldName));
          _initKey(doc, th.parent("tr"), fieldName);
        }

        $("<button type='button' class='apply'></button>").click(function() {
          applyChange();
        }).appendTo(tools);
        $("<button type='button' class='cancel'></button>").click(function() {
          cancelChange();
        }).appendTo(tools);
        tools.appendTo(th);
        input.val(fieldName).appendTo(th);
        input.each(function() { this.focus(); this.select(); });
      }

      function _editValue(doc, cell, fieldName) {
        if (!fieldName || fieldName == "_id" || fieldName == "_rev") return;
        var td = $(cell);
        var value = doc[fieldName];
        var needsTextarea = $("dl", td).length > 0 || $("code", td).text().length > 60;
        td.empty();
        if (needsTextarea) {
          var input = $("<textarea rows='8' cols='40' spellcheck='false'></textarea>");
        } else {
          var input = $("<input type='text' spellcheck='false'>");
        }
        input.dblclick(function() { return false; }).keydown(function(evt) {
          switch (evt.keyCode) {
            case 13: if (!needsTextarea) applyChange(); break;
            case 27: cancelChange(); break;
          }
        });
        var tools = $("<div class='tools'></div>");
        function applyChange() {
          input.nextAll().remove();
          try {
            var newValue = input.val() || "null";
            if (newValue == doc[fieldName]) {
              cancelChange();
              return;
            }
            doc[fieldName] = JSON.parse(newValue);
            td.children().remove();
            page.isDirty = true;
            var value = _renderValue(doc[fieldName]);
            td.append(value);
            _initValue(value);
          } catch (err) {
            input.addClass("invalid");
            var msg = err.message;
            if (msg == "parseJSON") {
              msg = "Please enter a valid JSON value (for example, \"string\").";
            }
            $("<div class='error'></div>").text(msg).insertAfter(input);
          }
        }
        function cancelChange() {
          td.children().remove();
          var value = _renderValue(doc[fieldName]);
          td.append(value);
          _initValue(value);
        }

        $("<button type='button' class='apply' title='Apply change'></button>").click(function() {
          applyChange();
        }).appendTo(tools);
        $("<button type='button' class='cancel' title='Revert change'></button>").click(function() {
          cancelChange();
        }).appendTo(tools);
        tools.appendTo(td);
        input.val($.futon.formatJSON(value)).appendTo(td);
        input.each(function() { this.focus(); this.select(); });
        if (needsTextarea) input.makeResizable({vertical: true});
      }

      function _initKey(doc, row, fieldName) {
        if (fieldName != "_id" && fieldName != "_rev") {
          $("<button type='button' class='delete' title='Delete field'></button>").click(function() {
            delete doc[fieldName];
            row.remove();
            page.isDirty = true;
            $("#fields tbody.content tr").removeClass("odd").filter(":odd").addClass("odd");
          }).prependTo(row.find("th"));
        }
      }

      function _initValue(value) {
        value.find("dd:has(dl)").hide().prev("dt").addClass("collapsed");
        value.find("dd:not(:has(dl))").addClass("inline").prev().addClass("inline");
        value.find("dt.collapsed").click(function() {
          $(this).toggleClass("collapsed").next().toggle();
        });
      }

      function _renderValue(value) {
        var type = typeof(value);
        if (type == "object" && value !== null) {
          var list = $("<dl></dl>");
          for (var i in value) {
            if (!value.hasOwnProperty(i)) continue;
            $("<dt></dt>").text(i).appendTo(list);
            $("<dd></dd>").append(_renderValue(value[i])).appendTo(list);
          }
          return list;
        } else {
          return $("<code></code>").addClass(type).text(
            value !== null ? JSON.stringify(value) : "null"
          );
        }
      }

      function _renderAttachmentList(attachments) {
        var ul = $("<ul></ul>").addClass("attachments");
        $.each(attachments, function(idx, attachment) {
          _renderAttachmentItem(idx, attachment).appendTo(ul);
        });
        return ul;
      }

      function _renderAttachmentItem(name, attachment) {
        var attachmentHref = db.uri + encodeDocId(docId) 
          + "/" + encodeAttachment(name);
        var li = $("<li></li>");
        $("<a href='' title='Download file' target='_top'></a>").text(name)
          .attr("href", attachmentHref)
          .wrapInner("<tt></tt>").appendTo(li);
        $("<span>()</span>").text("" + $.futon.formatSize(attachment.length) + 
          ", " + attachment.content_type).addClass("info").appendTo(li);
        if (name == "tests.js") {
          li.find('span.info').append(', <a href="/_utils/couch_tests.html?' 
            + attachmentHref + '">open in test runner</a>');
        }
        _initAttachmentItem(name, attachment, li);
        return li;
      }

      function _initAttachmentItem(name, attachment, li) {
        $("<button type='button' class='delete' title='Delete attachment'></button>").click(function() {
          delete page.doc._attachments[name];
          li.remove();
          page.isDirty = true;
          return false;
        }).prependTo($("a", li));
      }
    }

  });

  function encodeDocId(docid) {
    var encoded, parts = docid.split('/');
    if (parts[0] == '_design') {
      parts.shift();
      encoded = encodeURIComponent(parts.join('/'));
      return '_design/'+encoded;
    } else {
      return encodeURIComponent(docid);
    }
  };

  function encodeAttachment(name) {
    var encoded = [], parts = name.split('/');
    for (var i=0; i < parts.length; i++) {
      encoded.push(encodeURIComponent(parts[i]));
    };
    return encoded.join('/');
  }

})(jQuery);
