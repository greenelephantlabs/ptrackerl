all:
	rebar get-deps && rebar compile

clean:
	rebar clean

build_plt: all
	rebar skip_deps=true build-plt

doc: all
	rebar skip_deps=true doc

xref: all
	rebar skip_deps=true xref

shell: all
	erl -pa ebin -pa deps/*/ebin +Bc +K true -smp enable -boot start_sasl -s crypto -s ibrowse -s ssl -s reloader -eval "c:m(ptrackerl)"
