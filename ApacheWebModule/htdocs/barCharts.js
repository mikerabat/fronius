function getTraceConf() {
	var trace1 = {
           x: [],
           y: [],
           type: 'bar',
           name: 'Power Consume',

           marker: {
              color: 'rgb(49,130,189)',
              opacity: 0.7
           }
         };


      var trace2 = {
           x: [],
           y: [],
           type: 'bar',
           name: 'Power Solar',

           marker: {
             color: 'rgb(204,204,204)',
             opacity: 0.5
           }
          };
	  var trace3 = {
           x: [],
           y: [],
           type: 'bar',
           name: 'Power Use',

           marker: {
             color: 'rgb(104,104,104)',
             opacity: 0.5
           }
          };

    var retVal = {"consumed":trace1, "produced":trace2, "solaruse":trace3};
	return retVal;
}


function updateDaysChart( mon, year )
{
  var dtStr = '&fromMon=' + mon + '&fromYear=' + year;
  dtStr = dtStr + '&toDay=0&toMon=' + mon + '&toYear=' + year;
  
  // ###########################################
  // #### fetch the current data
  $.ajax({
    url: 'https://' + shost + '/fronius/bars?barType=0' + dtStr,
	xhrFields: {
		           withCredentials: true
	           },				
    //crossDomain: true, 
    
    success: function(resp) {
      // process your data to pull out what you plan to use to update the chart
      // e.g. new label and a new data point
      var tr = getTraceConf();
	  
	  const cDaysInMon = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
      	
      // now process the data
	  traceIdx = 0;
	  var numDaysInMon = 31;
	  if(resp.data.length > 0) numDaysInMon = cDaysInMon[resp.data[traceIdx].sdi.month - 1];
	  for(var aDay = 1; aDay <= numDaysInMon; aDay++ )
	  {
		  tr.produced.x.push(aDay);
		  if( traceIdx < resp.data.length && resp.data[traceIdx].sdi.day == aDay )
		  {
			tr.produced.y.push(resp.data[traceIdx].enprod);
			tr.consumed.y.push(resp.data[traceIdx].enconsume );
			tr.solaruse.y.push(resp.data[traceIdx].enprod - resp.data[traceIdx].entogrid ); 
		    traceIdx++;
		  } else {
			tr.produced.y.push(0);
			tr.consumed.y.push(0);
			tr.solaruse.y.push(0);    
		  }
	  }
	  
	  tr.consumed.x = tr.produced.x;
	  tr.solaruse.x = tr.produced.x;
			

      var data = [tr.consumed, tr.produced, tr.solaruse];


      var layout = {
            title: resp.fromDi.year + ' Power Usage',
		    xaxis: {
              tickangle: 0,
			  dtick: 1
            },
            barmode: 'group'
          };


      // re-render the chart
	  var plt = document.getElementById('daysChart');
      Plotly.newPlot('daysChart', data, layout);
    }
  });
  
}

function updateMonthChart( year ) {
	var dt = '&fromDay=1&fromMon=1&fromYear=' + year;
	var dt = '&toDay=31&toMon=12&toYear=' + year;
	
	$.ajax({
    url: 'https://' + shost + '/fronius/bars?barType=1' + dt,
    xhrFields: {
		           withCredentials: true
	           },				
    //crossDomain: true, 
    
	success: function(resp) {
      // process your data to pull out what you plan to use to update the chart
      // e.g. new label and a new data point
      var tr = getTraceConf();
      	

      // now process the data
	  traceIdx = 0;
	  for(aMon = 1; aMon <= 12; aMon++ )
	  {
		  tr.produced.x.push(aMon);
		  if( traceIdx < resp.data.length && resp.data[traceIdx].sdi.month == aMon )
		  {
			tr.produced.y.push(resp.data[traceIdx].enprod);
			tr.consumed.y.push(resp.data[traceIdx].enconsume );
			tr.solaruse.y.push(resp.data[traceIdx].enprod - resp.data[traceIdx].entogrid ); 
		      
			traceIdx++;
		  } else {
			tr.produced.y.push(0);
			tr.consumed.y.push(0);
			tr.solaruse.y.push(0);    
		  }
	  }
	  
	  tr.consumed.x = tr.produced.x;
	  tr.solaruse.x = tr.produced.x;
			

      var data = [tr.consumed, tr.produced, tr.solaruse];
      var layout = {
            title: resp.fromDi.year + ' Power Usage',
		    xaxis: {
              tickangle: 0,
			  dtick: 1
            },
            barmode: 'group'
          };


      // re-render the chart
	  var plt = document.getElementById('monthChart');
      Plotly.newPlot('monthChart', data, layout);
    }
  });
}

function initBarCharts() {
  
  var d = new Date();
  updateDaysChart( d.getMonth() + 1, d.getFullYear() );
  updateMonthChart( d.getFullYear() );
  
  $.ajax({
    url: 'https://' + shost + '/fronius/bars?barType=2',
	xhrFields: {
		           withCredentials: true
	           },				
    //crossDomain: true, 
    
    success: function(resp) {
      // process your data to pull out what you plan to use to update the chart
      // e.g. new label and a new data point
      var tr = getTraceConf();
      	

      // now process the data
	  traceIdx = 0;
	  if(resp.data.length == 0) return;
	  
	  for(aYear = resp.data[0].sdi.year; aYear <= resp.data[resp.data.length - 1].sdi.year; aYear++ )
	  {
		  tr.produced.x.push(aYear);
		  if( traceIdx < resp.data.length && resp.data[traceIdx].sdi.year == aYear )
		  {
			tr.produced.y.push(resp.data[traceIdx].enprod);
			tr.consumed.y.push(resp.data[traceIdx].enconsume );
			tr.solaruse.y.push(resp.data[traceIdx].enprod - resp.data[traceIdx].entogrid ); 
		       
			traceIdx++;
		  } else {
			tr.produced.y.push(0);
			tr.consumed.y.push(0);
			tr.solaruse.y.push(0);    
		  }
	  }
	  
	  tr.consumed.x = tr.produced.x;
	  tr.solaruse.x = tr.produced.x;
			
      var data = [tr.consumed, tr.produced, tr.solaruse];
	  var layout = {
            title: resp.fromDi.year + ' Power Usage',
		    xaxis: {
              tickangle: 0,
			  dtick: 1
            },
            barmode: 'group'
          };


      // re-render the chart
	  var plt = document.getElementById('yearChart');
      Plotly.newPlot('yearChart', data, layout);
    }
  }); 
};