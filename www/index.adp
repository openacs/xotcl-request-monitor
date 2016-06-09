<master>
<property name="doc(title)">@title;literal@</property>

<if @jsGraph@ eq 1>
<property name="head">
<script type="text/javascript" src="//ajax.googleapis.com/ajax/libs/jquery/1.11.3/jquery.min.js"></script>
<script type="text/javascript" src="//code.highcharts.com/highcharts.js"></script>
<script type="text/javascript" src="//code.highcharts.com/modules/exporting.js"></script>
</property>
</if>

<table style="border: 0px solid blue; padding: 10px;">
  <tr><td><strong>Active Users:</strong></td><td>@active_user_string;noquote@</td><if @param_url@ ne "">
<td align="right"><a class="button" href="@param_url@">#acs-subsite.parameters#</a></td>
</if>
</tr>
  <tr><td><strong>Current System Activity:</strong></td><td>@current_system_activity@</td></tr>
  <tr><td><strong>Current System Load:</strong></td><td>@current_load@</td></tr>
  <tr><td><strong>Current Avg Response Time/sec:</strong></td><td>@current_response@</td></tr>
  <tr><td><strong>Threads:</strong></td><td>@running@ <a href='running'>Request(s)</a>
  currently running,  
  #connection threads @current_threads.current@ idle @current_threads.idle@,
  min @current_threads.min@ max @current_threads.max@
  <br>avg. #connection threads @thread_avgs.current@, avg. busy @thread_avgs.busy@</td></tr>
  <tr><td><strong>Summaries:</strong></td><td><a href='stat-details'>Aggregated URL</a> Statistics,
  <a href='last100'>Last 100</a> Requests,
  <a href='throttle-statistics'>Throttle</a> Statistics
</td><td><div style="font-size: 80%">
<a class='button' href="@toggle_graphics_url@">Toggle Graphics</a>
</div></td>
</tr>
</table>

<if @jsGraph@ eq 1> 
<table border='0'>
<tr><td colspan='2'><h3 style="margin-top:10px;">Page View Statistics</h3></td></tr>
@views_trend;noquote@

<tr><td colspan='2'><h3 style="margin-top:10px;">Active Users</h3></td></tr>
@users_trend;noquote@

<tr><td colspan='2'><h3 style="margin-top:10px;">Avg. Response Time in milliseconds</h3></td></tr>
@response_trend;noquote@

<tr><td colspan='2'><h3 style="margin-top:10px;">Throttle Statistics</h3></td></tr>
<tr><td>@throttle_stats;noquote@</td></tr>

</table>
<p>Detailed <a href='throttle-statistics'>Throttle statistics</a>

</if>
<else>
<h3 style='text-align: center;'>Page View Statistics</h3>
<div style="padding: 0px;">@views_trend;noquote@</div><p>

<h3 style='text-align: center;'>Active Users</h3>
<div style="padding: 0px;">@users_trend;noquote@</div><p>

<h3 style='text-align: center;'>Avg. Response Time in milliseconds</h3>
<div style="padding: 0px;">@response_trend;noquote@</div>

<div style="padding: 0px;">@throttle_stats;noquote@</div>
Detailed <a href='throttle-statistics'>Throttle statistics</a>
</else>

