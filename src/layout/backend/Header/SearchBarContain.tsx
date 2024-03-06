import Link from "next/link";
import { useTranslations } from "next-intl";
import { X } from "react-feather";
import { Form, Input } from "reactstrap";

import SvgIcon from "@/components/common/Icons/SvgIcon";
import { useLayout } from "@/hooks";
import { SearchBarContainPropsType } from "@/types/layout";

const SearchBarContain = ({
  handleSearch,
  suggestion,
  searchValue,
  setSearchValue,
  fieldTouch,
}: SearchBarContainPropsType) => {
  const { searchIcon, setSearchIcon } = useLayout();
  const t = useTranslations("translations");

  const handleOnClick = () => {
    if (typeof document !== "undefined")
      document.body.classList.remove("offcanvas");
    setSearchValue("");
  };

  return (
    <Form className={`form-inline search-full col ${searchIcon ? "open" : ""}`}>
      <div className="form-group w-100">
        <div className="Typeahead Typeahead--twitterUsers">
          <div className="u-posRelative">
            <Input
              id="searchBar"
              name="searchBar"
              onChange={handleSearch}
              value={searchValue}
              className="Typeahead-input form-control-plaintext w-100"
              placeholder={`${t("search")}...`}
            />
            <div className="spinner-border Typeahead-spinner">
              <span className="sr-only">{t("loading")}...</span>
            </div>
            <X
              onClick={() => setSearchIcon(!searchIcon)}
              className="close-search"
            />
          </div>
          <div className="Typeahead-menu is-open" id="search-outer">
            <div className="header-search-suggestion custom-scrollbar">
              {suggestion.map((item, i) => (
                <div className="ProfileCard u-cf" key={i}>
                  <div className="ProfileCard-details">
                    <div className="ProfileCard-realName">
                      <Link
                        onClick={handleOnClick}
                        className="realname  w-100 d-flex justify-content-start gap-2"
                        href={item.path}
                      >
                        <SvgIcon
                          className="stroke-icon"
                          iconId={`stroke-${item.icon}`}
                        />
                        {t(item.title)}
                      </Link>
                    </div>
                  </div>
                </div>
              ))}
            </div>
          </div>
          <div
            className={`Typeahead-menu empty-menu ${
              suggestion.length == 0 && fieldTouch ? "is-open" : ""
            } `}
          >
            <div className="tt-dataset tt-dataset-0">
              <div className="EmptyMessage">{t("noSearchResult")}</div>
            </div>
          </div>
        </div>
      </div>
    </Form>
  );
};

export default SearchBarContain;
