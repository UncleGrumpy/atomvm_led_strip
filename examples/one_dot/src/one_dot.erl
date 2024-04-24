%%
%% Copyright (c) 2024 <winford@object.stream>
%% All rights reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

-module(one_dot).

-include("config.hrl").

-export([start/0]).

-define(SLEEP_MS, 0).

start() ->
    ok = dotstar:init(?LED_DI_PIN, ?LED_CI_PIN, ?TOTAL_DOTS),
    ok = dotstar:write_dot(255, 0, 0, 100),
    timer:sleep(1000),
    ok = dotstar:write_dot(0, 255, 0, 100),
    timer:sleep(1000),
    ok = dotstar:write_dot(0, 0, 200, 100),
    timer:sleep(1000),
    fade_rgb(255, 255, 255, 100),
    hsv_sweep(0, 100, 20),
    random_loop().

fade_rgb(Red, Green, Blue, 0) ->
    ok = dotstar:write_dot(Red, Green, Blue, 0),
    ok;
fade_rgb(Red, Green, Blue, Illum) ->
    ok = dotstar:write_dot(Red, Green, Blue, Illum),
    timer:sleep(36),
    Fade = Illum - 1,
    fade_rgb(Red, Green, Blue, Fade).

hsv_sweep(259, S, 100) ->
    dotstar:write_dot_hsv(259, S, 100),
    timer:sleep(?SLEEP_MS),
    ok;   
hsv_sweep(359, S, V) ->
    dotstar:write_dot_hsv(259, S, V),
    timer:sleep(?SLEEP_MS),
    hsv_sweep(0, S, V + 2);
hsv_sweep(H, S, V) ->
    dotstar:write_dot_hsv(H, S, V),
    timer:sleep(?SLEEP_MS),
    hsv_sweep((H + 1) rem 360, S, V).

random_loop() ->
    Rand = crypto:strong_rand_bytes(5),
    <<R:8, G:8, B:8, I:8, T:8>> = Rand,
    Illum = (I * 100) div 255,
    ok = dotstar:write_dot(R, G, B, Illum),
    timer:sleep(T),
    random_loop().

