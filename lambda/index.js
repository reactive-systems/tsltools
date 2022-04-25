const { exec, execSync } = require('child_process');
const fs = require('fs');

callServerless = function(query) {

	var output = ""
	timeoutLength = 1;
	var ltlsynthCommand = 'bash -c \"echo \\"' + query + '\\" | ./tslsynth \"'
	//bash -c "echo \\"'+ query + '\\" | ./tslsynth"';
	try {
		console.log("starting process");
		output = execSync(ltlsynthCommand, { timeout: timeoutLength * (1000), detached: true, killSignal: 'SIGKILL' }).toString();
	} catch (e) {
		console.error("failed (could be timeout)");
		console.error(e);
		return "unknown";
	}
	if (output.trim() == "unknown") {
		console.log("couldnt find a repair");
		return "unknown";
	}
	return output;

}

exports.handler = async (event) => {
  console.log(event);
  data = event.query;
  if (data == undefined) {
    console.log("trying to read as API call")
    data = JSON.parse(event.body).query;
  }
  const response = {
      statusCode: 200,
      body: JSON.stringify(callServerless(data)),
  };
  return response;
};
