<property name="&doc">doc</property>
<property name="context">@context;literal@</property>
<master>

<h1>Long Calls</h1>
<div style="float:right">
&nbsp;Lines: <a href="long-calls?lines=20">20</a>,
<a href="long-calls?lines=50">50</a>,
<a href="long-calls?lines=100">100</a>,
<a href="long-calls?lines=200">200</a>,
<a href="long-calls?lines=500">500</a>,
<a href="long-calls?lines=1000">1000</a>,
<a href="long-calls?lines=2000&amp;readsize=100000">2000</a>,
<a href="long-calls?lines=5000&amp;readsize=500000">5000</a>,
<a href="long-calls?lines=10000&amp;readsize=500000">10000</a>
&nbsp;
</div>

<table class="table table-condensed table-bordered small">
    <thead>
      <tr>
        <th class='text-right'>Queuetime</th>
        <th class='text-right'>Filtertime</th>
	<th class='text-right'>Runtime</th>
	<th class='text-right'>Totaltime</th>
        <th>Date</th>
        <th>User ID</th>
        <th>IP</th>
        <th>URL</th>
      </tr>
    </thead>
    <tbody>
    @rows;noquote@
    </tbody>
</table>

	      