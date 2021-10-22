import React, { useEffect } from 'react';
import './App.css';
import ReactMarkdown from 'react-markdown';

type SearchResult = {
  Hover: HoverResult;
  Definition: Element;
  DefRanges: Element[];
  Moniker: Element;
  Others: Element[];
};

type HoverResult = {
  Id: number;
  Contents: string;
  rng: LSRange | null;
}

type Element = {
  Id: number;
  Label: string;
  Type: 'vertex' | 'edge';
  Payload: any;
}

type LSRange = {
  Start: Position;
  End: Position;
}

type Position = {
  Line: number;
  Character: number; 
}

const SearchBar = ({ onSearchTermChange }: { onSearchTermChange: (searchTerm: string) => void }) => {
  return <input type="text" placeholder="Search" onChange={event => onSearchTermChange(event.target.value)} />
};

const SearchResults = ({ searchResults }: { searchResults: SearchResult[] }) => {
  return <div>
    {searchResults.map((searchResult, i) => <Entry index={i} searchResult={searchResult} />)}
  </div>
};

const Entry = ({ index, searchResult }: { index: number, searchResult: SearchResult }) => {
  return <div>
    <h2>Result {index}</h2>
    <h3>Hovers</h3>
    <ReactMarkdown children={searchResult.Hover.Contents} />
    <h3>Definition</h3>
    <p>{JSON.stringify(searchResult.Definition.Payload)}</p>
    <ul>
      {searchResult.DefRanges.map((defRange, i) => <li key={i}>{JSON.stringify(defRange.Payload)}</li>)}
    </ul>
    <h3>Moniker</h3>
    <p>{JSON.stringify(searchResult.Moniker.Payload)}</p>
    <h3>Others</h3>
    <ul>
      {searchResult.Others.map((other, i) => <li key={i}>{JSON.stringify(other.Payload)}</li>)}
    </ul>
  </div>
};

function App() {
  const endpoint = "http://localhost:8080/api/search/"
  const [searchTerm, setSearchTerm] = React.useState('');
  const [searchResults, setSearchResults] = React.useState([] as SearchResult[]);

  useEffect(() => {
    if (searchTerm.length > 0) {
      fetch(endpoint + "?q=" + searchTerm, { mode: 'cors' })
        .then(response => response.json())
        .then(data => setSearchResults(data))
        .catch(error => {
          console.error(error)
          setSearchResults([])
        })
    }
  }, [searchTerm]);

  return (
    <div>
      <SearchBar onSearchTermChange={setSearchTerm} />
      <p>{searchTerm.length > 0 ? `Searching for ${searchTerm}` : 'Please enter a search term'}</p>
      <SearchResults searchResults={searchResults} />
    </div>
  );
}

export default App;
