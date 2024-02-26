import { Fragment, useState } from "react";
import { MenuList as Menulist } from "./MenuList";
import { MenuList } from "../menu";
import { ArrowLeft, ArrowRight } from "react-feather";
import { usePathname } from "@/navigation";
import { useCustomizer, useLayout } from "@/hooks";
import Theme from "@/configs/theme";
import { SidebarMenuType } from "@/types/layout";
import { useTranslations } from "next-intl";

export const SideBarMenu = () => {
  const { pinedMenu } = useLayout();
  const { layoutName } = useCustomizer();
  const wrapper = Theme.data.settings.layout_class;
  const [margin, setMargin] = useState(0);
  const [leftArrow, setLeftArrow] = useState(true);
  const [rightArrow, setRightArrow] = useState(false);
  const pathname = usePathname();
  const [active, setActive] = useState(pathname ? pathname : "");
  const [prev, setPrev] = useState<number | undefined>();
  const [activeLink, setActiveLink] = useState<string | undefined>(
    active.split("/")[active.split("/").length - 1]
  );

  const handleActive = (title: string, level: number) => {
    if (active.includes(title)) {
      if (active.includes("/")) {
        const tempt = active.split("/");
        tempt.splice(level, tempt.length - level);
        setActive(tempt.join("/"));
        setPrev(level);
      } else {
        setActive("");
      }
    } else {
      if (level < active.split("/").length) {
        if (level == prev) {
          const tempt = active.split("/");
          tempt.splice(level, 1, title);
          setActive(tempt.join("/"));
        } else {
          setActive(title);
        }
      } else {
        setPrev(level);
        const tempt = active;
        const concatString = tempt.concat(`/${title}`);
        setActive(concatString);
      }
    }
  };
  const scrollToRight = () => {
    if (margin === 0) {
      setMargin((margin) => (margin += -1000));
      setLeftArrow(false);
    } else if (margin === -1000) {
      setMargin((margin) => (margin += -1000));
    } else if (margin === -2000) {
      setMargin((margin) => (margin += -1000));
      setRightArrow(true);
    }
  };
  const scrollToLeft = () => {
    if (margin === -1000) {
      setMargin(0);
      setLeftArrow(true);
      setRightArrow(false);
    } else if (margin === -2000) {
      setMargin((margin) => (margin -= -1000));
    } else if (margin === -3000) {
      setMargin((margin) => (margin -= -1000));
      setRightArrow(false);
    }
  };
  const shouldHideMenu = (mainMenu: SidebarMenuType) => {
    return mainMenu.Items.map((data) => data.title).every((tittles) =>
      pinedMenu.includes(tittles || "")
    );
  };

  const t = useTranslations();

  return (
    <nav className="sidebar-main">
      {wrapper === "horizontal-wrapper" ||
      layoutName == "losangeles" ||
      "singapore" ? (
        <div
          className={`left-arrow ${leftArrow ? "disabled" : ""}`}
          id="left-arrow"
          onClick={scrollToLeft}
        >
          <ArrowLeft />
        </div>
      ) : (
        ""
      )}
      <div
        id="sidebar-menu"
        style={
          wrapper === "horizontal-wrapper" ||
          layoutName == "losangeles" ||
          "singapore"
            ? { marginLeft: margin + "px" }
            : { margin: "0px" }
        }
      >
        <ul className="sidebar-links custom-scrollbar" id="simple-bar">
          <div className="simplebar-wrapper">
            <div className="simplebar-mask">
              <div className="simplebar-offset">
                <div className="simplebar-content-wrapper">
                  <div className="simplebar-content">
                    <li className="back-btn">
                      <div className="mobile-back text-end">
                        {" "}
                        <span>{"Back"}</span>{" "}
                        <i
                          className="fa fa-angle-right ps-2"
                          aria-hidden="true"
                        ></i>
                      </div>
                    </li>
                    <li
                      className={`pin-title sidebar-main-title ${
                        pinedMenu.length > 1 ? "show" : ""
                      } `}
                    >
                      <div>
                        <h6>Pinned</h6>
                      </div>
                    </li>
                    {MenuList &&
                      MenuList.map((mainMenu, i) => (
                        <Fragment key={i}>
                          <li
                            className={`sidebar-main-title ${
                              shouldHideMenu(mainMenu) ? "d-none" : ""
                            }`}
                          >
                            <div>
                              <h6 className="lan-1">
                                {t(`${mainMenu.title}`)}
                              </h6>
                            </div>
                          </li>
                          <Menulist
                            setActive={setActive}
                            setActiveLink={setActiveLink}
                            activeLink={activeLink}
                            handleActive={handleActive}
                            active={active}
                            MENUITEMS={mainMenu.Items}
                            level={0}
                          />
                        </Fragment>
                      ))}
                  </div>
                </div>
              </div>
            </div>
          </div>
        </ul>
      </div>
      {wrapper === "horizontal-wrapper" ||
      layoutName == "losangeles" ||
      "singapore" ? (
        <div
          className={`right-arrow ${rightArrow ? "disabled" : ""}`}
          onClick={scrollToRight}
        >
          <ArrowRight />
        </div>
      ) : (
        ""
      )}
    </nav>
  );
};