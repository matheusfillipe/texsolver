from flask import Flask, request

from texsuggest import solve

app = Flask(__name__)
app.config["DEBUG"] = True

@app.route("/", methods=["POST"])
def index():
    if request.method == "POST":
        expression = request.json.get("latex")
        try:
            return {"latex": solve(expression)}
        except Exception as e:
            return {"error": str(e)}


if __name__ == "__main__":
    app.run()
