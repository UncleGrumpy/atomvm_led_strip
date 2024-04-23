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

-define(SPI_START_FRAME, 16#00000000).
-define(SPI_END_FRAME, 16#FFFFFFFF).
-define(ILUM_START_BITS, 2#111).
-define(LED_BRIGHTNESS_MSB, 16#E0).
-define(SPI_CLOCK_HZ, 40000000).
-define(LED_DEVICE_NAME, ap102).
-define(COLOR_ORDER, {Blue, Green, Red}).
