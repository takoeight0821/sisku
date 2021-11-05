import React, { useEffect } from 'react';
import './App.css';
import ReactMarkdown from 'react-markdown';
import Container from '@mui/material/Container';
import TextField from '@mui/material/TextField';
import Typography from '@mui/material/Typography';

type SearchResult = {
  hover: HoverResult;
  definition: Element;
  defRanges: Element[];
  moniker: Element;
};

type HoverResult = {
  id: number;
  result: { contents: { kind: string, value: string } };
  rng: LSRange | null;
}

type Element = {
  id: number;
  label: string;
  type: 'vertex' | 'edge';
  payload: any;
}

type LSRange = {
  start: Position;
  end: Position;
}

type Position = {
  line: number;
  character: number;
}

const SearchBar = ({ onSearchTermChange }: { onSearchTermChange: (searchTerm: string) => void }) => {
  return <TextField label="Search" onChange={(e) => onSearchTermChange(e.target.value)} />
};

const SearchResults = ({ searchResults }: { searchResults: SearchResult[] }) => {
  return <div>
    {searchResults.map((searchResult, i) =>
      <Entry key={i} index={i} searchResult={searchResult} />
    )}
  </div>
};

const Entry = ({ index, searchResult }: { index: number, searchResult: SearchResult }) => {
  return <>
    <Typography variant="h2">Result {index}</Typography>
    <Typography variant="h3">Hovers</Typography>
    <ReactMarkdown children={searchResult.hover.result.contents.value} />
    <Typography variant="h3">Definition</Typography>
    <Typography>{JSON.stringify(searchResult.definition)}</Typography>
    <ul>
      {searchResult.defRanges.map((defRange, i) =>
        <li key={i}>
          <Typography>{JSON.stringify(defRange)}</Typography>
        </li>
      )}
    </ul>
    <Typography variant="h3">Moniker</Typography>
    <Typography>{JSON.stringify(searchResult.moniker)}</Typography>
  </>
};

function App() {
  const endpoint = "http://localhost:8080/search"
  const [searchTerm, setSearchTerm] = React.useState('');
  const [searchResults, setSearchResults] = React.useState([] as SearchResult[]);

  useEffect(() => {
    if (searchTerm.length > 0) {
      fetch(endpoint + "?q=" + searchTerm, { mode: 'cors' })
        .then(response => response.json())
        .then(data => { console.log(data); return data; })
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
