import { ReactNode, useEffect } from "react";

import { MenuListData } from "@/configs/menu";
import { THEME_CUSTOMIZER_ENABLED } from "@/environment";
import { useAppSelector, useAuth, useCustomizer, useLayout } from "@/hooks";
import { ChildrenProps } from "@/types/common";
import { SearchableMenuType, SidebarItemType } from "@/types/layout";

import { Spinner } from "../../components/common/Spinner";
import { TapTop } from "../../components/common/TapTop";
import { Footer } from "./Footer";
import { Header } from "./Header";
import { SideBar } from "./Sidebar";
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
  const { loading, theme } = useAppSelector((state) => state.theme);

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

    if (theme === "light") {
      document.body.classList.remove("dark-only");
      document.body.classList.add("light-only");
    } else {
      document.body.classList.remove("light-only");
      document.body.classList.add("dark-only");
    }

    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [theme]);

  return loading ? (
    <Spinner />
  ) : (
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
