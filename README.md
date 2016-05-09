logi_fluentd
============

A [logi](https://github.com/sile) implementation library for [fluentd](http://www.fluentd.org/)

Build
-----

The build tool is [rebar3](https://github.com/erlang/rebar3).

Add following code to your `rebar.config` and `.app.src` file:
```erlang
%% In rebar.config
{deps,
 [
   {logi_fluentd, {git, "https://github.com/sile/logi_fluentd.git", {tag, "0.1.0"}}}
 ]}.

%% In src/${YOUR_APP}.app.src
{application, ${YOUR+APP},
  [
    {applications,
      [
        %% other dependencies
        logi_fluentd
      ]}
  ]}.
```

API
---

See [EDoc Documents](doc/README.md)

Example
-------

Add following lines to your td-agent.conf and start td-agent (fluentd):

```
<source>
  tag default
  type tcp
  format json
  port 9880
  time_key timestamp
</source>
```

```erlang
$ rebar3 shell
> Sink = logi_fluentd_sink_tcp:new(sample_sink, "localhost", [{port, 9880}]).
> {ok, _} = logi_channel:install_sink(Sink, info).
> logi:info("Hello World!").
```

License
-------

This library is released under the MIT License.

See the [LICENSE](LICENSE) file for full license information.
