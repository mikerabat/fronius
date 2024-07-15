/* Datepicker functions used here... */

function createDatePicker( ref, onPick, incr )
{
	var refDiv = $( "#" + ref );
	
	var imgL = $("<img>",{src:"./img/arrow-square-left.svg"}).width(32).height(32);
	var imgR = $("<img>",{src:"./img/arrow-square-right.svg"}).width(32).height(32);
   
    var altField = $('<div/>');
	var dtField = $('<div/>');
	
	var maxZ = 0;
	// that does not work??
	$( "div" ).each(function( index, element ) {
		var z = parseInt( $(this).css( "z-index" ), 10 );
		if( !isNaN(z)) maxZ = Math.max(maxZ, z);
	});
	
	maxZ = maxZ + 1000;
	
	dtField.css({ "position"  :   "absolute",
                "top"       :   0,
                "left"      :   0,
                "display"   :   "none",
                "width"     :   "auto",
				"height"    :   "auto",
				"overflow"  :   "auto",
				"background-color"  :   "#F00"
        });
	dtField.hide();
	
	var date = new Date();
    var today = new Date();
	
	function _pickeroptions() {
		var ret = {
		            showOn: "both",
                    buttonImage: "./img/calendar-icon24.png",
				    buttonImageOnly: true,
				    buttonText:"Select date",
				    showWeek : true,
				    showAnim:"slide",
				    onclose: function ( data ) {
					  dtField.hide();
					},
					dateFormat : "dd.mm.yy",
				  };
				  
		if( incr == 1 ) {
			ret.changeYear = true;
			ret.changeMonth = true;
			ret.dateFormat = 'MM yy';
		}
		if( incr == 2 ) {
			ret.changeYear = true;
			ret.changeMonth = false;
			ret.dateFormat = 'yy';
		}
		
		return ret;
	}
	
	altField.text(date.getDate() + '.' + (date.getMonth()+1) + '.' + date.getFullYear());
	altField.on("click", 
	     function () {
            var z = parseInt( $( this ).css( "z-index" ), 10 );
            
			// Set the box that was clicked to one higher that the z-index that was clicked
            dtField.css("z-index", maxZ );	
			var position = $( this ).position();			
			dtField.css("left", position.left );
			dtField.css("top", position.top + $(this).height() );
			
		    dtField.show(); 
			var opt = _pickeroptions();
			 
	        var dtPick = dtField.datepicker({
		          opt,
				  
				  onSelect: function ( data ) {
					          date = dtPick.datepicker("getDate");
 					          altField.text(date.getDate() + '.' + (date.getMonth()+1) + '.' + date.getFullYear());
	                          dtField.hide();
	
							  onPick(date);
				            }
				  
	            });
				
			dtField.css("left", position.left - dtPick.width() + altField.width() );
				
			dtPick.datepicker( 'setDate', date );
	} );
	
		
	imgL.on('click', 
	         function () {
                 if(incr == 1) date.setMonth( date.getMonth() - 1 );
				 else if(incr == 2) date.setFullYear( date.getFullYear() - 1 );
				 else date.setDate( date.getDate() - 1);
				 
				 altField.text(date.getDate() + '.' + (date.getMonth()+1) + '.' + date.getFullYear());
	
				 onPick(date); 
	         } );		
	imgR.on('click', function () {
                 if(incr == 1) date.setMonth( date.getMonth() + 1 );
				 else if(incr == 2) date.setFullYear( date.getFullYear() + 1 );
				 else date.setDate( date.getDate() + 1);
				 altField.text(date.getDate() + '.' + (date.getMonth()+1) + '.' + date.getFullYear());
	              
				 onPick(date);				 
	         } );
		
	/*var dtPick = inputField.datepicker({
               	dateFormat : 'dd/mm/yy',
				showOn: "both",
                buttonImage: "img/calendar-icon24.png",
	            buttonImageOnly: true,
                buttonText: "Select date",
                duration: "slow",
			    onSelect: onPick,
			    showOn: "both",
               
				changeMonth: true,
                changeYear: true,
                yearRange: "-100:+0"
            }); */

/*			   dayNamesMin: [ "Su", "Mo", "Tu", "We", "Th", "Fr", "Sa" ],
               dateFormat:"dd.mm.yyyy",
			   duration: "slow",
			   onSelect: onPick,
			   showOn: "both",
               buttonImage: "img/calendar-icon24.png",
               buttonImageOnly: true,
               buttonText: "Select date"
            });*/
	 refDiv.append(imgL);
     refDiv.append(altField); 
     refDiv.append(imgR);
	 refDiv.append(dtField);
}