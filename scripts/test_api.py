import json
import sys
from typing import Literal
import requests
from requests import Response

def json_response(response: Response) -> str:
        if 200 <= response.status_code < 300:
            return json.dumps(response.json(), indent=4)
        else:
            return f"Error {response.status_code}: {response.content}"

class Client:
    def __init__(self, host: str, email: str, pw: str):
        self.__host = host
        self.__session = requests.Session()
        self.__login(email, pw)

    def __login(self, email: str, pw: str):
        payload = {
            "loginEmail": email,
            "loginPassword": pw
        }
        response = self.post("login", payload)

        if 200 <= response.status_code < 300:
            print("Successfully logged in!")
        else:
            print(f"Error: {response.status_code}")

    def document(self, doc_id: int) -> str:
        return self.get_json(f"v2/docs/{doc_id}")

    def document_tree(self, doc_id: int, revision: Literal["latest"] | int) -> str:
        return self.get_json(f"v2/docs/{doc_id}/tree/{revision}")

    def document_tree_full(self, doc_id: int, revision: Literal["latest"] | int) -> str:
        return self.get_json(f"v2/docs/{doc_id}/tree/{revision}/full")

    def document_history(self, doc_id: int) -> str:
        return self.get_json(f"v2/docs/{doc_id}/history")

    def post(self, endpoint: str, payload: object) -> Response:
        return self.__session.post(
            url=f"{self.__host}/{endpoint}",
            json=payload,
            headers=self.__headers()
        )

    def get(self, endpoint: str) -> Response:
        return self.__session.get(
            url=f"{self.__host}/{endpoint}",
            headers=self.__headers()
        )

    def get_json(self, endpoint: str) -> str:
        return json_response(self.get(endpoint))

    def post_json(self, endpoint: str, payload: object) -> str:
        return json_response(self.post(endpoint, payload))

    def __headers(self) -> dict[str, str]:
        xsrf_token = self.__session.cookies.get("XSRF-TOKEN")
        headers = {
            "X-XSRF-TOKEN": xsrf_token or "",
            "Accept": "application/json",
            "Referer": self.__host,
        }
        print(f"{headers=}")
        return headers


if __name__ == "__main__":
    client = Client(
        "http://localhost:8080/api"
            if "--local" in sys.argv
            else "https://batailley.informatik.uni-kiel.de/api",
        "test@test.com",
        "123"
    )
    print(client.document(1))
    print(client.document_tree_full(1, "latest"))
    print(client.document_history(1))
