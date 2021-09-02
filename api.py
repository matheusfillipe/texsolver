from pprint import pformat

from flask import Flask, flash, jsonify, request

app = Flask(__name__)
app.config["DEBUG"] = True

# logging
@app.before_request
def log_request_info():
    app.logger.debug("Headers: %s", request.headers)
    try:
        app.logger.debug("Body: %s", pformat(request.get_json()))
    except Exception as e:
        app.logger.debug("ERROR PARSING JSON: %s", str(e))


@app.route("/", methods=["POST"])
def index():
    if request.method == "POST":
        # questions = request.json.get("questions")
        return request.get_json()


if __name__ == "__main__":
    app.run()
