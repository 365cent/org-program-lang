package main

import (
	"fmt"
	"io/ioutil"
	"net/http"
	"net/url"
	"strings"

	"golang.org/x/net/html"
)

func fetch(url string) (string, []string, error) {
	resp, err := http.Get(url)
	if err != nil {
		return "", nil, err
	}
	defer resp.Body.Close()

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		return "", nil, err
	}

	pageContent := string(body)
	links, err := parseLinks(url, pageContent)
	if err != nil {
		return "", nil, err
	}

	return pageContent, links, nil
}

func parseLinks(baseURL, htmlContent string) ([]string, error) {
	links := []string{}
	doc, err := html.Parse(strings.NewReader(htmlContent))
	if err != nil {
		return nil, err
	}
	var f func(*html.Node)
	f = func(n *html.Node) {
		if n.Type == html.ElementNode && n.Data == "a" {
			for _, a := range n.Attr {
				if a.Key == "href" {
					link, err := url.Parse(a.Val)
					if err != nil {
						continue
					}
					base, err := url.Parse(baseURL)
					if err != nil {
						continue
					}
					links = append(links, base.ResolveReference(link).String())
				}
			}
		}
		for c := n.FirstChild; c != nil; c = c.NextSibling {
			f(c)
		}
	}
	f(doc)

	return links, nil
}

func saveHTML(url, content string) error {
	// Sanitize url to create filename
	fileName := strings.Replace(url, "https://", "", -1)
	// remove host and keep original filename
	fileName = strings.Replace(fileName, "www.csd.uwo.ca/~mmorenom/CS447/Lectures/Lexical.html/", "", -1)

	return ioutil.WriteFile(fileName, []byte(content), 0644)
}

func crawl(url string, visited map[string]bool) {
	if visited[url] {
		return
	}
	visited[url] = true

	content, links, err := fetch(url)
	if err != nil {
		fmt.Println("Failed to fetch:", url, err)
		return
	}

	err = saveHTML(url, content)
	if err != nil {
		fmt.Println("Failed to save:", url, err)
		return
	}

	for _, link := range links {
		crawl(link, visited)
	}
}

func main() {
	visited := make(map[string]bool)
	startURL := "https://www.csd.uwo.ca/~mmorenom/CS447/Lectures/Lexical.html/Lexical.html"
	crawl(startURL, visited)
}
