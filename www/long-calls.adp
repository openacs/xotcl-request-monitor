<master>
  <property name="&doc">doc</property>
  <property name="context">@context;literal@</property>

  <h1>Long Calls</h1>
  <div style="float:left">
    <form style="float:left;" action="./long-calls" class="form-inline w3-padding-small">
      <div class="w3-cell-row">
        <span class="w3-cell">
          Connection Pools:&nbsp;
        </span>
        <multiple name="poolcheckboxes">
          <div class="form-check form-check-inline form-group w3-cell w3-padding-small">
            <input class="form-check-input w3-check"
                   type="checkbox"
                   id="inlineCheckbox-@poolcheckboxes.name@"
                   name="pool"
                   value="@poolcheckboxes.name@" @poolcheckboxes.checked@>
            <label class="form-check-label"
                   for="inlineCheckbox-$pool">@poolcheckboxes.name@</label>
          </div>
        </multiple>
        <span class="w3-cell">
          <button type="submit" class="btn <adp:class name='btn-outline-secondary'>">
            Filter
          </button>
        </span>
        <input type="hidden" name="lines" value="@lines@">
        <input type="hidden" name="by_starttime" value="@by_starttime@">
      </div>
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
    <a href="long-calls?lines=2000&amp;readsize=500000@filterQuery;literal@">2000</a>,
    <a href="long-calls?lines=5000&amp;readsize=1000000@filterQuery;literal@">5000</a>,
    <a href="long-calls?lines=10000&amp;readsize=2000000@filterQuery;literal@">10000</a>
    &nbsp;
  </div>

  <table class="table table-condensed table-bordered small w3-table-all">
    <thead>
      <tr>
        <th class='text-right' nowrap>
          <a href="@toggle_request_start_url@"
             title="@toggle_request_time_title@">@request_time_label@</a>
        </th>
        <th class='text-right' nowrap>
          <a href="@base_sort_url@&order=queuetime">
            Queuetime<if @ordersign_queuetime@ not nil> @ordersign_queuetime@</if>
          </a>
        </th>
        <th class='text-right' nowrap>
          <a href="@base_sort_url@&order=filtertime">
            Filtertime<if @ordersign_filtertime@ not nil> @ordersign_filtertime@</if>
          </a>
        </th>
        <th class='text-right' nowrap>
          <a href="@base_sort_url@&order=runtime">
            Runtime<if @ordersign_runtime@ not nil> @ordersign_runtime@</if>
          </a>
        </th>
        <th class='text-right' nowrap>
          <a href="@base_sort_url@&order=totaltime">
            Totaltime<if @ordersign_totaltime@ not nil> @ordersign_totaltime@</if>
          </a>
        </th>
        <th nowrap>User ID</th>
        <th>IP</th>
        <th>Pool</th>
        <th>URL</th>
      </tr>
    </thead>
    <tbody>
      @rows;noquote@
    </tbody>
  </table>
