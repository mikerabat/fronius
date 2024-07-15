

function onStatusTimeout() {
  // ###########################################
  // #### fetch the current data
  $.ajax({
    url: 'https://' + shost + '/fronius/cursamp',
	xhrFields: {
		           withCredentials: true
	           },				
    //crossDomain: true, 
                
    success: function(resp) {
		
	  var ts = document.getElementById('statusTimeStamp');
	  var powerPV = document.getElementById('powersolar');
      var powerGrid = document.getElementById('powergrid');
      var powerUse = document.getElementById('poweruse');		
		
	  if(resp.error && resp.error == -1) {
		  ts.innerHTML = '<h4>Failed to fetch data.</h4>';
		  powerPV.innerHTML = '';
	      powerGrid.innerHTML = '';
	      powerUse.innerHTML = '';	
	  
	      window.setTimeout( onStatusTimeout, 2000 );
		  return;
	  }
	  
	  // error -2 -> not logged in redirect to login
	  if(resp.error && resp.error == -2) {
		  ts.innerHTML = '<h4>Not logged in - redirect to login</h4>';
		  powerPV.innerHTML = '';
	      powerGrid.innerHTML = '';
	      powerUse.innerHTML = '';	
	  
	      window.replace('/login/index.html');
		  return;
	  }
		
		
	    
	  if(resp.error) {
		  ts.innerHTML = '<h4>Error: ' + resp.msg + '</h4>';
		  powerPV.innerHTML = '';
		  powerGrid.innerHTML = '';
		  powerUse.innerHTML = '';
	  } else {
		  ts.innerHTML = '<h4>' + resp.t + '</h4>';
		  powerPV.innerHTML = '<b>' + resp.PPV + ' W</b>';
		  powerGrid.innerHTML = '<b>' + resp.PG + ' W</b>';
		  powerUse.innerHTML = '<b>' + resp.PL + ' W</b>';
	  }
	  
	  // reload again
	  window.setTimeout( onStatusTimeout, 2000 );
	},
	error: function(xhr, status, error ) {
	  var ts = document.getElementById('statusTimeStamp');
	  var powerPV = document.getElementById('powersolar');
      var powerGrid = document.getElementById('powergrid');
      var powerUse = document.getElementById('poweruse');	  
	  	
	  ts.innerHTML = '<h3>Error: ' + error + '</h3>';
	  powerPV.innerHTML = '';
	  powerGrid.innerHTML = '';
	  powerUse.innerHTML = '';	
	  
	  window.setTimeout( onStatusTimeout, 2000 );
	}	
  });
}


function initPVStatusView() {
   window.setTimeout( onStatusTimeout, 2000 );
};