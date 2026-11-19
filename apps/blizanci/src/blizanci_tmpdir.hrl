%% blizanci, a Gemini protocol server, by Martin Keegan
%%
%% SPDX-License-Identifier: Apache-2.0

%% Every temporary file blizanci creates starts with this, so that
%% blizanci_tmpdir:stale/2 can tell them apart from anything else in the
%% directory.
-define(PREFIX, "blizanci-tmp-").
