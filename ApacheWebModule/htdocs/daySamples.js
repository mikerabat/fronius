function pl(elem) {
	return elem.PL;
}

function pg(elem) {
	return elem.PG;
}

function ppv(elem) {
	return elem.PPV;
}

function times(elem) {
	return elem.t;
}

var timeLabels = ["0:00","2:00","4:00","6:00","8:00", "10:00", "12:00", "14:00", "16:00", 
                  "18:00", "20:00", "22:00", "24:00"];
		
		
function updateDaySample( dt ) {
	var ds = dt.getDate() + '.' + (dt.getMonth() + 1) + '.' + dt.getFullYear();
	
	// ###########################################
    // #### fetch the current data
  $.ajax({
    url: 'https://' + shost + '/fronius/daysamples?date=' + ds,
	xhrFields: {
		           withCredentials: true
	           },				
    //crossDomain: true, 
    
    success: function(resp) {
      // process your data to pull out what you plan to use to update the chart
      // e.g. new label and a new data point
      
      // add new label and data point to chart's underlying data structures
      //myChart.data.labels.push("Post " + postId++);
      //myChart.data.datasets[0].data.push(getRandomIntInclusive(1, 25));
	  
	  var tracepl = {x:[], y:[], mode:'lines', name:'Load', fill:'tozeroy',hoverinfo:'y',hovertemplate:'<b>Load %{y:%d}</b><extra></extra>'};
	  var traceppv = {x:[], y:[], mode:'lines', name:'PV', fill:'tozeroy',hoverinfo:'y',hovertemplate:'<b>PV %{y:%d}</b><extra></extra>'};
	  var tracepg = {x:[], y:[], mode:'lines', name:'grid', fill:'tozeroy',hoverinfo:'y',hovertemplate:'<b>Grid %{y:%d}</b><extra></extra>'};
	  
	  tracepl.x = resp.data.map(times);
	  traceppv.x = tracepl.x;
	  tracepg.x = tracepl.x;
	  
	  tracepl.y = resp.data.map( pl );
	  traceppv.y = resp.data.map( ppv );
	  tracepg.y = resp.data.map( pg );
	  
	  var tickValues = [];
	  var i = 0;
	  while(i <= 86400) {
		  tickValues.push(i);
		  i += 7200;
	  }
	  
	  var data = [/*traceBase, */tracepl, traceppv, tracepg];
	  var layout = {
		  title: 'Daily samples',
		  hovermode:'closest',
		  xaxis:{
			range:[0,86400],
			autorange:false,
			dtick:7200,
			tickmode:'array',
			tickvals:tickValues,
			ticktext:timeLabels
		  }
	  }
	  
      // re-render the chart
	  var plt = document.getElementById('dayChart');
      Plotly.newPlot('dayChart', data, layout);
    }
  });
}

function initDayChart() {
  
  var dt = new Date();
  
  updateDaySample( dt );
  /*setInterval(function(){
    updateData(data);
	myNewChart.data = data;
	myNewChart.update();
    ;}, 2000
  );*/
};