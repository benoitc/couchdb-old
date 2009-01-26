% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_httpd_stats_handlers).
-include("couch_db.hrl").

-export([handle_stats_req/1]).
-import(couch_httpd,
    [send_json/2,send_json/3,send_json/4,send_method_not_allowed/2,
    start_json_response/2,send_chunk/2,end_json_response/1,
    start_chunked_response/3, send_error/4]).

handle_stats_req(#httpd{method='GET'}=Req) ->
  OpenDbs = couch_stats_aggregator:get({<<"couch_db">>, <<"open_databases">>}),
  Response = {[{<<"couch_db">>, {[{<<"open_databases">>, OpenDbs}]}}]},
  send_json(Req, Response);
handle_stats_req(Req) ->
  send_method_not_allowed(Req, "GET").
