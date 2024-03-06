import SvgIcon from "@/components/common/Icons/SvgIcon";
import { useLayout } from "@/hooks";

export const SearchBar = () => {
  const { searchIcon, setSearchIcon } = useLayout();

  return (
    <li>
      <span className="header-search">
        <SvgIcon
          iconId="search"
          onClick={() => {
            setSearchIcon(!searchIcon);
          }}
        />
      </span>
    </li>
  );
};
