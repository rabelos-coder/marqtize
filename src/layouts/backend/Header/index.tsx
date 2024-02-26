import { Row } from "reactstrap";

import { useLayout } from "@/hooks";

import { LeftBar } from "./LeftBar";
import { RightBar } from "./RightBar";
import Search from "./Search";

export const Header = () => {
  const { sideBarToggle } = useLayout();

  return (
    <div className={`page-header ${sideBarToggle ? "close_icon" : ""}`}>
      <Row className="header-wrapper m-0">
        <Search />
        <LeftBar />
        <RightBar />
      </Row>
    </div>
  );
};
