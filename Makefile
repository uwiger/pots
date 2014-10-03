# %CopyrightBegin%
#
# Copyright Erlang Solutions Ltd 2010. All Rights Reserved.
#
# The contents of this file are subject to the Erlang Public License,
# Version 1.1, (the "License"); you may not use this file except in
# compliance with the License. You should have received a copy of the
# Erlang Public License along with this software. If not, it can be
# retrieved online at http://www.erlang.org/.
#
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
# the License for the specific language governing rights and limitations
# under the License.
#
# %CopyrightEnd%
#
all:
	mkdir -p ebin
	(cd src; make)
	(cd study; make)

clean:
	rm -f *~ \#*\#
	(cd src; make clean)
	(cd study; make clean)

orig:
	(mkdir -p ebin; cd study/orig; make run)

event_fifo:
	(mkdir -p ebin; cd study/event_fifo; make run)

event_fifo_asynch:
	(mkdir -p ebin; cd study/event_fifo_asynch_1; make run)

event_sel:
	(mkdir -p ebin; cd study/event_sel_asynch_1; make run)
