import { ChangeEvent, useState } from "react";
import SearchBarContain from "./SearchBarContain";
import { useLayout } from "@/hooks";
import { SearchableMenuType } from "@/types/layout";

const Search = () => {
  const [suggestion, setSuggestion] = useState<SearchableMenuType[]>([]);
  const [searchValue, setSearchValue] = useState("");
  const [fieldTouch, setFieldTouch] = useState(false);
  const { searchableMenu } = useLayout();

  const handleSearch = (event: ChangeEvent<HTMLInputElement>) => {
    const searchKey = event.target.value.toLowerCase();
    setFieldTouch(true);
    setSearchValue(event.target.value);
    if (searchKey !== "") {
      if (typeof document !== "undefined")
        document.body.classList.add("offcanvas");
      const search = searchableMenu.filter((item) => {
        return item.title.toLowerCase().includes(searchKey);
      });
      setSuggestion(search);
    }
    if (searchKey === "") {
      if (typeof document !== "undefined")
        document.body.classList.remove("offcanvas");
      setSuggestion([]);
    }
  };
  return (
    <SearchBarContain
      handleSearch={handleSearch}
      suggestion={suggestion}
      searchValue={searchValue}
      setSearchValue={setSearchValue}
      fieldTouch={fieldTouch}
      setFieldTouch={setFieldTouch}
    />
  );
};

export default Search;
