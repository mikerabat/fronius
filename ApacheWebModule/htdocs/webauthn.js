var serverSettings = {};
var fidoBaseUrl = '';

function getFidoSettings() {
    $.ajax({
        //url: 'auth/FidoWebAuth.exe/settings',
		url: '../fronius/settings',
        contentType: "application/json; charset=utf-8",
        dataType: "json",
		xhrFields: {
		  withCredentials: true
	    },
		// crossDomain: true,
        success: function (response) {
			//alert( JSON.stringify( response ) );
			serverSettings = response;
			fidoBaseUrl = 'https://' + serverSettings.publicKey.rp.id;
			fidoBaseUrl += '/fronius/';
			
			
			if( serverSettings.AllowEnroll ) $('#register-paragraph').show()
			else $('#register-paragraph').hide();
		
			if( serverSettings.CanLogin ) $('#login-button').show()
		    else $('#login-button').hide();
		    //$('#login-button').show();
            
			console.log(response);
        }
    });	
}

function showCred() {
	$.ajax({
		url: fidoBaseUrl + 'get',
        contentType: "application/json; charset=utf-8",
        dataType: "json",
        xhrFields: {
		  withCredentials: true
	    },
		// crossDomain: true,
		success: function (response) {
			console.log(response);
			
			var cr = response.response;
			
			var attestObj = bufferDecodeUrl( cr.attestationObject );
			
		    var attestDecode = CBOR.decode( attestObj.buffer );
			
			var authda = CBOR.decode( attestDecode.authData.buffer );
			var auth2 = CBOR.decode( authda.authData.buffer );
			console.log( JSON.stringify(auth2) );
        }
    });	
}

function detectWebAuthnSupport() {
    if (window.PublicKeyCredential === undefined ||
        typeof window.PublicKeyCredential !== "function") {
        $('#register-button').attr("disabled", true);
        $('#login-button').attr("disabled", true);
        var errorMessage = "Oh no! This browser doesn't currently support WebAuthn.";
		
        if (window.location.protocol === "http:" && (window.location.hostname !== "localhost" && window.location.hostname !== "127.0.0.1")){
            errorMessage = "WebAuthn only supports secure connections. For testing over HTTP, you can use the origin \"localhost\".";
        }
        showErrorAlert(errorMessage);
        return;
    }
}

function string2buffer(str) {
    return (new Uint8Array(str.length)).map(function (x, i) {
        return str.charCodeAt(i)
    });
}

// Encode an ArrayBuffer into a base64 string.
function bufferEncode(value) {
    return base64js.fromByteArray(value)
        .replace(/\+/g, "-")
        .replace(/\//g, "_")
        .replace(/=/g, "");
}


function bufferDecodeUrl( input ) {
	// Replace non-url compatible chars with base64 standard chars
        input = input
            .replace(/-/g, '+')
            .replace(/_/g, '/');

        // Pad out with standard base64 required padding characters
        var pad = input.length % 4;
        if(pad) {
          if(pad === 1) {
            throw new Error('InvalidLengthError: Input base64url string is the wrong length to determine padding');
          }
          input += new Array(5-pad).join('=');
        }
		
	return bufferDecode(input);
	
}

// Don't drop any blanks
// decode
function bufferDecode(value) {
    return Uint8Array.from(atob(value), c => c.charCodeAt(0));
}

function buffer2string(buf) {
    let str = "";
    if (!(buf.constructor === Uint8Array)) {
        buf = new Uint8Array(buf);
    }
    buf.map(function (x) {
        return str += String.fromCharCode(x)
    });
    return str;
}

var state = {
    createResponse: null,
    publicKeyCredential: null,
    credential: null,
    user: {
        name: "testuser@example.com",
        displayName: "testuser",
    },
}

function setUser() {
    username = $("#input-email").val();
    state.user.name = username.toLowerCase().replace(/\s/g, '');
    state.user.displayName = username.toLowerCase();
}

function checkUserExists() {
    //$.get('auth/FidoWebAuth.exe/userexists?uname=' + state.user.name, {}, null, 'json')
    $.get(fidoBaseUrl + 'userexists?uname=' + state.user.name, {}, null, 'json')
          .done(function (response) {
            return response.result == 2;
        }).catch(function () {
            return false;
        });
}

function getCredentials() {
    //$.get('auth/FidoWebAuth.exe/credential/' + state.user.name, {}, null, 'json')
    $.get(fidoBaseUrl + 'credential?uname=' + state.user.name, {}, null, 'json')
        .done(function (response) {
            console.log(response)
        });
}

function makeCredential() {
    hideErrorAlert();
    console.log("Fetching options for new credential");
    if ($("#input-email").val() === "") {
        showErrorAlert("Please enter a username");
        return;
    }
    setUser();
    var credential = null;

    var attestation_type = serverSettings.publicKey.attest
	//$('#select-attestation').find(':selected').val();
    var authenticator_attachment = serverSettings.publicKey.authenticatorSelection.authenticatorAttachment;
	//$('#select-authenticator').find(':selected').val();
    
    var user_verification = serverSettings.publicKey.authenticatorSelection.userVerification;
	//$('#select-verification').find(':selected').val();
    var resident_key_requirement = serverSettings.publicKey.authenticatorSelection.requireResidentKey;
	//$('#select-residency').find(':selected').val();
    var txAuthSimple_extension = 0;// $('#extension-input').val();

    //$.get('auth/FidoWebAuth.exe/enrollstart?uname=' + state.user.name, {  // actually...we only need the uname...
    $.get(fidoBaseUrl + 'enrollstart?uname=' + state.user.name, {  // actually...we only need the uname...
            attType: attestation_type,
            authType: authenticator_attachment,
            userVerification: user_verification,
            residentKeyRequirement: resident_key_requirement,
            txAuthExtension: txAuthSimple_extension,
        }, null, 'json')
        .done(function (makeCredentialOptions) {            
            makeCredentialOptions.publicKey.challenge = bufferDecodeUrl(makeCredentialOptions.publicKey.challenge);
            makeCredentialOptions.publicKey.user.id = bufferDecodeUrl(makeCredentialOptions.publicKey.user.id);
            if (makeCredentialOptions.publicKey.excludeCredentials) {
                for (var i = 0; i < makeCredentialOptions.publicKey.excludeCredentials.length; i++) {
                    makeCredentialOptions.publicKey.excludeCredentials[i].id = bufferDecode(makeCredentialOptions.publicKey.excludeCredentials[i].id);
                }
            }
            console.log("Credential Creation Options");
            console.log(makeCredentialOptions);
            navigator.credentials.create({
                publicKey: makeCredentialOptions.publicKey
            }).then(function (newCredential) {
                console.log("PublicKeyCredential Created");
                console.log(newCredential);
                state.createResponse = newCredential;
                registerNewCredential(newCredential);
            }).catch(function (err) {
                console.info(err);
            });
        });
}

// This should be used to verify the auth data with the server
function registerNewCredential(newCredential) {
    // Move data into Arrays incase it is super long
    let attestationObject = new Uint8Array(newCredential.response.attestationObject);
    let clientDataJSON = new Uint8Array(newCredential.response.clientDataJSON);
    let rawId = new Uint8Array(newCredential.rawId);

	
    $.ajax({
        url: fidoBaseUrl + 'enrollVerify',
        type: 'POST',
        xhrFields: {
		  withCredentials: true
	    },
		// crossDomain: true,
		data: JSON.stringify({
            id: newCredential.id,
            rawId: bufferEncode(rawId),
            type: newCredential.type,
            response: {
                attestationObject: bufferEncode(attestationObject),
                clientDataJSON: bufferEncode(clientDataJSON),
            },
        }),
        contentType: "application/json; charset=utf-8",
        dataType: "json",
        success: function (response) {
			if( response.error !== undefined ) {
				alert('An error occured while enrolling the credentials: ' + response.msg );
			} else {
				if(response.verified) window.location.replace('https://' + serverSettings.publicKey.rp.id + '/index.html');
				else alert('Credentials could not be verified');
			}
        }
    });
}

function addUserErrorMsg(msg) {
    if (msg === "username") {
        msg = 'Please add username';
    } else {
        msg = 'Please add email';
    }
    document.getElementById("user-create-error").innerHTML = msg;
}

function DoAssert() {
	var user_verification = $('#select-verification').find(':selected').val();            
	var txAuthSimple_extension = $('#extension-input').val();

	$.get(fidoBaseUrl + 'assertstart?uname=' + state.user.name, {
		userVer: user_verification,
		txAuthExtension: txAuthSimple_extension
	}, null, 'json')
	.done(function (makeAssertionOptions) {
		console.log("Assertion Options:");
		console.log(makeAssertionOptions);
		makeAssertionOptions.publicKey.challenge = bufferDecodeUrl(makeAssertionOptions.publicKey.challenge);
		makeAssertionOptions.publicKey.allowCredentials.forEach(function (listItem) {
			listItem.id = bufferDecodeUrl(listItem.id)
		});
		console.log(makeAssertionOptions);
		navigator.credentials.get({
				publicKey: makeAssertionOptions.publicKey
			})
			.then(function (credential) {
				console.log(credential);
				verifyAssertion(credential);
			}).catch(function (err) {
				console.log(err.name);
				showErrorAlert(err.message);
			});
	});
}

function getAssertion() {
    hideErrorAlert();
    if ($("#input-email").val() === "") {
        showErrorAlert("Please enter a username");
		return;
    }
    setUser();
	
	/*$.ajax({
        url: fidoBaseUrl + 'createSession',
        type: 'POST',
        xhrFields: {
		  withCredentials: true
	    },
		// crossDomain: true,
		contentType: "application/json; charset=utf-8",
        dataType: "json",
        success: function (response) {
            if(response.verified) 
			{ 
		        window.location.replace('https://' + serverSettings.publicKey.rp.id);
			}
            else showErrorAlert("Login failed with error: " + response.msg);
			
			console.log(response)
        }
    });*/
	
	$.get(fidoBaseUrl + 'userexists?uname=' + state.user.name, {}, null, 'json')
	.done(function (response) {
		if(response.result == 2) {
			DoAssert();
		} else {
			showErrorAlert(response.msg);
		}
	}).catch(function () {
		return false;
	});
}

function verifyAssertion(assertedCredential) {
    // Move data into Arrays incase it is super long
    console.log('calling verify')
    let authData = new Uint8Array(assertedCredential.response.authenticatorData);
    let clientDataJSON = new Uint8Array(assertedCredential.response.clientDataJSON);
    let rawId = new Uint8Array(assertedCredential.rawId);
    let sig = new Uint8Array(assertedCredential.response.signature);
    let userHandle = new Uint8Array(assertedCredential.response.userHandle);
    $.ajax({
        url: fidoBaseUrl + 'assertverify',
        type: 'POST',
        xhrFields: {
		  withCredentials: true
	    },
		// crossDomain: true,
		data: JSON.stringify({
            id: assertedCredential.id,
            rawId: bufferEncode(rawId),
            type: assertedCredential.type,
            response: {
                authenticatorData: bufferEncode(authData),
                clientDataJSON: bufferEncode(clientDataJSON),
                signature: bufferEncode(sig),
                userHandle: bufferEncode(userHandle),
            },
        }),
        contentType: "application/json; charset=utf-8",
        dataType: "json",
        success: function (response) {
            if(response.verified) 
			{ 
		        window.location.replace('https://' + serverSettings.publicKey.rp.id);
			}
            else showErrorAlert("Login failed with error: " + response.msg);
			
			console.log(response)
        }
    });
}

function setCurrentUser(userResponse) {
    state.user.name = userResponse.name;
    state.user.displayName = userResponse.display_name;
}

function showErrorAlert(msg) {
	alert(msg);
   // $("#alert-msg").text(msg)
   // $("#alert").show();
}

function hideErrorAlert() {
    $("#alert").hide();
}

function popoverPlacement(context, source) {
    if ($(window).width() < 992) {
        return "bottom"
    }
    return "right";
}
