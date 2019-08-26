import requests as re

def request_func1(token = "866de98d0d47426e92cc0e3394df5f07", base="optim.uni-muenster.de:5000/",
                  endpoint = "api-test2D", func = 1, x = 1, y = 1):

    if endpoint == "api":
        return print("access denied")

    call = str("http://" + base + "/" + endpoint + "/" + str(func) + "/" + token + "/" + str(x) + "," + str(y))
    print(call)
    contents = re.get(call)
    print(contents.text)

request_func1()
