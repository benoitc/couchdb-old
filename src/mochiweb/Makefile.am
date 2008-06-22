## Licensed under the Apache License, Version 2.0 (the "License"); you may not
## use this file except in compliance with the License.  You may obtain a copy
## of the License at
##
##   http://www.apache.org/licenses/LICENSE-2.0
##
## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
## WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
## License for the specific language governing permissions and limitations under
## the License.

devdocdir = $(localdocdir)/developer/mochiweb

mochiwebebindir = $(localerlanglibdir)/mochiweb-r76/ebin

mochiwebebin_DATA = $(static_files) $(compiled_files)

dist_devdoc_DATA = $(doc_base) $(doc_modules)

EXTRA_DIST = $(static_files) $(source_files)

CLEANFILES = $(compiled_files) $(doc_base) $(doc_modules) edoc-info

static_files = mochiweb.app

source_files = \
    mochifmt.erl \
    mochifmt_records.erl \
    mochifmt_std.erl \
    mochihex.erl \
    mochijson.erl \
    mochijson2.erl \
    mochinum.erl \
    mochiweb.erl \
    mochiweb_app.erl \
    mochiweb_charref.erl \
    mochiweb_cookies.erl \
    mochiweb_echo.erl \
    mochiweb_headers.erl \
    mochiweb_html.erl \
    mochiweb_http.erl \
    mochiweb_multipart.erl \
    mochiweb_request.erl \
    mochiweb_response.erl \
    mochiweb_skel.erl \
    mochiweb_socket_server.erl \
    mochiweb_sup.erl \
    mochiweb_util.erl \
    reloader.erl

compiled_files = \
    mochifmt.beam \
    mochifmt_records.beam \
    mochifmt_std.beam \
    mochihex.beam \
    mochijson.beam \
    mochijson2.beam \
    mochinum.beam \
    mochiweb.beam \
    mochiweb_app.beam \
    mochiweb_charref.beam \
    mochiweb_cookies.beam \
    mochiweb_echo.beam \
    mochiweb_headers.beam \
    mochiweb_html.beam \
    mochiweb_http.beam \
    mochiweb_multipart.beam \
    mochiweb_request.beam \
    mochiweb_response.beam \
    mochiweb_skel.beam \
    mochiweb_socket_server.beam \
    mochiweb_sup.beam \
    mochiweb_util.beam \
    reloader.beam

doc_base = \
    erlang.png \
    index.html \
    modules-frame.html \
    overview-summary.html \
    packages-frame.html \
    stylesheet.css

doc_modules = \
    mochifmt.html \
    mochifmt_records.html \
    mochifmt_std.html \
    mochihex.html \
    mochijson.html \
    mochijson2.html \
    mochinum.html \
    mochiweb.html \
    mochiweb_app.html \
    mochiweb_charref.html \
    mochiweb_cookies.html \
    mochiweb_echo.html \
    mochiweb_headers.html \
    mochiweb_html.html \
    mochiweb_http.html \
    mochiweb_multipart.html \
    mochiweb_request.html \
    mochiweb_response.html \
    mochiweb_skel.html \
    mochiweb_socket_server.html \
    mochiweb_sup.html \
    mochiweb_util.html \
    reloader.html

$(dist_devdoc_DATA): $(source_files)
	for file in $(source_files); do \
	    if test -n "$$edoc_files"; then \
	        edoc_files="$$edoc_files, "; \
	    fi; \
	    edoc_files="$$edoc_files \"$$file\""; \
	done; \
	erl -noshell -run edoc_run files ["$$edoc_files"]

%.beam: %.erl
	$(ERLC) $<
