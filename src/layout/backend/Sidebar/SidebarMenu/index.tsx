import { useTranslations } from "next-intl";
import { Fragment, useState } from "react";
import { ArrowLeft, ArrowRight } from "react-feather";

import { MenuListData } from "@/configs/menu";
import Theme from "@/configs/theme";
import { useAbility, useCustomizer, useLayout } from "@/hooks";
import { usePathname } from "@/navigation";
import { SidebarMenuType } from "@/types/layout";

import { MenuList } from "./MenuList";

export const SidebarMenu = () => {
  const { pinnedMenu } = useLayout();
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
  const t = useTranslations();
  const ability = useAbility();

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
    return mainMenu.items
      .map((data) => data.title)
      .every((titles) => pinnedMenu?.includes(titles || ""));
  };

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
                        <span>{t("back")}</span>
                        <i
                          className="fa fa-angle-right ps-2"
                          aria-hidden="true"
                        ></i>
                      </div>
                    </li>
                    <li
                      className={`pin-title sidebar-main-title ${
                        pinnedMenu?.length > 1 ? "show" : ""
                      } `}
                    >
                      <div>
                        <h6>{t("pinned")}</h6>
                      </div>
                    </li>
                    {MenuListData &&
                      MenuListData.map((mainMenu, i) =>
                        !mainMenu?.claims ||
                        (mainMenu?.claims?.length &&
                          mainMenu?.claims?.some((claim) => {
                            const [subject, action] = claim.split(":");

                            return ability?.can(action, subject);
                          })) ? (
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
                            <MenuList
                              setActive={setActive}
                              setActiveLink={setActiveLink}
                              activeLink={activeLink}
                              handleActive={handleActive}
                              active={active}
                              menuItems={mainMenu.items}
                              level={0}
                            />
                          </Fragment>
                        ) : (
                          <Fragment key={i} />
                        )
                      )}
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
