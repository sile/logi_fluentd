

# Module logi_fluentd_sink_tcp #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Fluentd Sink for the "in_tcp" Input Plugin.

Copyright (c) 2015-2016 Takeru Ohta <phjgt308@gmail.com>

<a name="types"></a>

## Data Types ##




### <a name="type-address">address()</a> ###


<pre><code>
address() = <a href="inet.md#type-ip_address">inet:ip_address()</a> | <a href="inet.md#type-hostname">inet:hostname()</a>
</code></pre>




### <a name="type-option">option()</a> ###


<pre><code>
option() = {logger, <a href="logi.md#type-logger">logi:logger()</a>} | {layout, <a href="logi_layout.md#type-layout">logi_layout:layout()</a>} | {port, <a href="inet.md#type-port_number">inet:port_number()</a>} | {connect_timeout, timeout()} | {connect_opt, [<a href="gen_tcp.md#type-connect_option">gen_tcp:connect_option()</a>]}
</code></pre>

`logger`:
-  The logger instance which is used to report internal events of the sink process
- Default: `logi:default_logger()`

`layout`:
- The layout instance used by the sink
- Default: `logi_fluentd_layout_json:new()`

`port`:
- The port number of the destination fluentd
- Default: `24224`

`connect_timeout`:
- Timeout of `gen_tcp:connect/4`
- Default: `1000`

`connect_opt`:
- Connect Options (i.e., the third argment of `gen_tcp:connect/4`)
- Default: `[]`



### <a name="type-options">options()</a> ###


<pre><code>
options() = [<a href="#type-option">option()</a>]
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#new-2">new/2</a></td><td>Equivalent to <a href="#new-3"><tt>new(SinkId, Address, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td>Creates a new sink.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(SinkId::<a href="logi_sink.md#type-id">logi_sink:id()</a>, Address::<a href="#type-address">address()</a>) -&gt; <a href="logi_sink.md#type-sink">logi_sink:sink()</a>
</code></pre>
<br />

Equivalent to [`new(SinkId, Address, [])`](#new-3).

<a name="new-3"></a>

### new/3 ###

<pre><code>
new(SinkId::<a href="logi_sink.md#type-id">logi_sink:id()</a>, Address::<a href="#type-address">address()</a>, Options::<a href="#type-options">options()</a>) -&gt; <a href="logi_sink.md#type-sink">logi_sink:sink()</a>
</code></pre>
<br />

Creates a new sink

