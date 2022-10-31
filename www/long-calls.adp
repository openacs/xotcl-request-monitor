<property name="&doc">doc</property>
<property name="context">@context;literal@</property>
<master>

<h1>Long Calls</h1>
<div style="float:left">
<form action="./long-calls" class="form-inline">
Connection Pools:&nbsp;
  <multiple name="poolcheckboxes">
    <div class="form-check form-check-inline form-group">
      <input class="form-check-input" type="checkbox" id="inlineCheckbox-@poolcheckboxes.name@" name="pool" value="@poolcheckboxes.name@" @poolcheckboxes.checked@>
      <label class="form-check-label" for="inlineCheckbox-$pool">@poolcheckboxes.name@</label>
    </div>
  </multiple>
  <button type="submit" class="btn btn-outline-secondary">Filter</button>
  <input type="hidden" name="lines" value="@lines@">
</form>
</div>

<div style="float:right">
&nbsp;Lines:
<a href="long-calls?lines=20@filterQuery;literal@">20</a>,
<a href="long-calls?lines=50@filterQuery;literal@">50</a>,
<a href="long-calls?lines=100@filterQuery;literal@">100</a>,
<a href="long-calls?lines=200@filterQuery;literal@">200</a>,
<a href="long-calls?lines=500@filterQuery;literal@">500</a>,
<a href="long-calls?lines=1000@filterQuery;literal@">1000</a>,
<a href="long-calls?lines=2000&amp;readsize=100000@filterQuery;literal@">2000</a>,
<a href="long-calls?lines=5000&amp;readsize=500000@filterQuery;literal@">5000</a>,
<a href="long-calls?lines=10000&amp;readsize=500000@filterQuery;literal@">10000</a>
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
        <th>Pool</th>
        <th>URL</th>
      </tr>
    </thead>
    <tbody>
    @rows;noquote@
    </tbody>
</table>
