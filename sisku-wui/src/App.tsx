import React, { useEffect } from 'react';
import './App.css';
import ReactMarkdown from 'react-markdown';
import Container from '@mui/material/Container';
import TextField from '@mui/material/TextField';
import Typography from '@mui/material/Typography';

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
  return <TextField label="Search" onChange={(e) => onSearchTermChange(e.target.value)} />
};

const SearchResults = ({ searchResults }: { searchResults: SearchResult[] }) => {
  return <div>
    {searchResults.map((searchResult, i) =>
      <Entry index={i} searchResult={searchResult} />
    )}
  </div>
};

const Entry = ({ index, searchResult }: { index: number, searchResult: SearchResult }) => {
  return <>
    <Typography variant="h2">Result {index}</Typography>
    <Typography variant="h3">Hovers</Typography>
    <ReactMarkdown children={searchResult.Hover.Contents} />
    <Typography variant="h3">Definition</Typography>
    <Typography>{JSON.stringify(searchResult.Definition.Payload)}</Typography>
    <ul>
      {searchResult.DefRanges.map((defRange, i) =>
        <li key={i}>
          <Typography>{JSON.stringify(defRange.Payload)}</Typography>
        </li>
      )}
    </ul>
    <Typography variant="h3">Moniker</Typography>
    <Typography>{JSON.stringify(searchResult.Moniker.Payload)}</Typography>
    <Typography variant="h3">Others</Typography>
    <ul>
      {searchResult.Others.map((other, i) => <li key={i}>
        <Typography>{JSON.stringify(other.Payload)}</Typography>
      </li>)}
    </ul>
  </>
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
    <Container sx={{ paddingTop: 2 }}>
      <SearchBar onSearchTermChange={setSearchTerm} />
      <Typography>{searchTerm.length > 0 ? `Searching for ${searchTerm}` : 'Please enter a search term'}</Typography>
      <SearchResults searchResults={searchResults} />
    </Container>
  );
}

export default App;
