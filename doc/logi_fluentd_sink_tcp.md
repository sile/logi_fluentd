

# Module logi_fluentd_sink_tcp #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Fluentd Sink for the "in_tcp" Input Plugin.

Copyright (c) 2015 Takeru Ohta <phjgt308@gmail.com>

__Behaviours:__ [`logi_sink`](logi_sink.md).

<a name="types"></a>

## Data Types ##




### <a name="type-address">address()</a> ###


<pre><code>
address() = <a href="inet.md#type-ip_address">inet:ip_address()</a> | <a href="inet.md#type-hostname">inet:hostname()</a>
</code></pre>




### <a name="type-writer_id">writer_id()</a> ###


<pre><code>
writer_id() = atom()
</code></pre>

 The identifier of a fluentd writer



### <a name="type-writer_option">writer_option()</a> ###


<pre><code>
writer_option() = {logger, <a href="logi.md#type-logger">logi:logger()</a>} | {port, <a href="inet.md#type-port_number">inet:port_number()</a>} | {connect_timeout, timeout()} | {connect_opt, [<a href="gen_tcp.md#type-connect_option">gen_tcp:connect_option()</a>]}
</code></pre>




### <a name="type-writer_options">writer_options()</a> ###


<pre><code>
writer_options() = [<a href="#type-writer_option">writer_option()</a>]
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Creates a new sink instance.</td></tr><tr><td valign="top"><a href="#start_writer-2">start_writer/2</a></td><td>Equivalent to <a href="#start_writer-3"><tt>start_writer(WriterId, Address, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#start_writer-3">start_writer/3</a></td><td>Starts a new fluentd writer.</td></tr><tr><td valign="top"><a href="#stop_writer-1">stop_writer/1</a></td><td>Stops the fluentd writer.</td></tr><tr><td valign="top"><a href="#which_writers-0">which_writers/0</a></td><td>Returns a list of the running fluentd writers.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(WriterId::<a href="#type-writer_id">writer_id()</a>) -&gt; <a href="logi_sink.md#type-sink">logi_sink:sink()</a>
</code></pre>
<br />

Creates a new sink instance

<a name="start_writer-2"></a>

### start_writer/2 ###

<pre><code>
start_writer(WriterId::<a href="#type-writer_id">writer_id()</a>, Address::<a href="#type-address">address()</a>) -&gt; {ok, pid()} | {error, Reason::term()}
</code></pre>
<br />

Equivalent to [`start_writer(WriterId, Address, [])`](#start_writer-3).

<a name="start_writer-3"></a>

### start_writer/3 ###

<pre><code>
start_writer(WriterId::<a href="#type-writer_id">writer_id()</a>, Address::<a href="#type-address">address()</a>, Options::<a href="#type-writer_options">writer_options()</a>) -&gt; {ok, pid()} | {error, Reason::term()}
</code></pre>
<br />

Starts a new fluentd writer

The default layout is `TODO`.

<a name="stop_writer-1"></a>

### stop_writer/1 ###

<pre><code>
stop_writer(WriterId::<a href="#type-writer_id">writer_id()</a>) -&gt; ok
</code></pre>
<br />

Stops the fluentd writer

If the writer does not exists, it is silently ignored.

<a name="which_writers-0"></a>

### which_writers/0 ###

<pre><code>
which_writers() -&gt; [<a href="#type-writer_id">writer_id()</a>]
</code></pre>
<br />

Returns a list of the running fluentd writers

