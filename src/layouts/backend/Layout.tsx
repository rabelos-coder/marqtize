import { ReactNode, useEffect } from "react";

import { MenuListData } from "@/configs/menu";
import { THEME_CUSTOMIZER_ENABLED } from "@/environment";
import { useAuth, useCustomizer, useLayout } from "@/hooks";
import { ChildrenProps } from "@/types/common";
import { SearchableMenuType, SidebarItemType } from "@/types/layout";

import { Footer } from "./Footer";
import { Header } from "./Header";
import { SideBar } from "./SideBar";
import { TapTop } from "./TapTop";
import { ThemeCustomizer } from "./ThemeCustomizer";

export const Layout = ({ children }: ChildrenProps) => {
  const { layout, setLayout } = useCustomizer();
  const {
    sideBarToggle,
    setSideBarToggle,
    setSearchableMenu,
    setBookmarkList,
  } = useLayout();
  const { user } = useAuth();

  const compactSidebar = () => {
    if (layout === "compact-wrapper") {
      if (typeof window !== "undefined" && window.innerWidth <= 1006) {
        setSideBarToggle(true);
      } else {
        setSideBarToggle(false);
      }
    } else if (layout === "horizontal-wrapper") {
      if (typeof window !== "undefined" && window.innerWidth <= 1006) {
        setSideBarToggle(true);
        setLayout("compact-wrapper");
      } else {
        setSideBarToggle(false);
        setLayout("horizontal-wrapper");
      }
    }
  };

  useEffect(() => {
    compactSidebar();
    if (typeof window !== "undefined")
      window.addEventListener("resize", () => {
        compactSidebar();
      });
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [layout]);

  useEffect(() => {
    const suggestionArray: SearchableMenuType[] = [];
    const bookmarkArray: SearchableMenuType[] = [];
    let num = 0;

    const getAllLink = (item: SidebarItemType, icon: ReactNode) => {
      if (item.children) {
        item.children.map((ele: SidebarItemType) => {
          getAllLink(ele, icon);
        });
      } else {
        num = num + 1;
        suggestionArray.push({
          icon: icon,
          title: item.title ? item.title : "",
          path: item.path ? item.path : "",
          bookmarked: item.bookmark ? item.bookmark : false,
          id: num,
        });
        if (item.bookmark) {
          bookmarkArray.push({
            icon: icon,
            title: item.title ? item.title : "",
            path: item.path ? item.path : "",
            bookmarked: item.bookmark,
            id: num,
          });
        }
      }
    };

    MenuListData.forEach((item) => {
      item.items?.map((child) => {
        getAllLink(child, child.icon);
      });
    });
    setSearchableMenu(suggestionArray);
    setBookmarkList(bookmarkArray);
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  return (
    <>
      <div
        className={`page-wrapper ${sideBarToggle ? "compact-wrapper" : layout}`}
      >
        <Header />
        <div className="page-body-wrapper">
          <SideBar />
          {children}
          <Footer />
        </div>
      </div>
      {THEME_CUSTOMIZER_ENABLED && user?.isSuperAdmin && <ThemeCustomizer />}
      <TapTop />
    </>
  );
};
