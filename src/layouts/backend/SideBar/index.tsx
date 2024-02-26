import Image from "next/image";

import Theme from "@/configs/theme";
import { useCustomizer, useLayout } from "@/hooks";
import { Link } from "@/navigation";

import { SideBarLogo } from "./SidebarLogo";
import { SideBarMenu } from "./SidebarMenu";

export const SideBar = () => {
  const { sidebarIconType } = useCustomizer();
  const { sideBarToggle } = useLayout();

  const IconType = sidebarIconType || Theme.data.settings.sidebar.iconType;

  return (
    <div
      className={`sidebar-wrapper ${sideBarToggle ? "close_icon" : ""}`}
      sidebar-layout={IconType}
    >
      <div>
        <SideBarLogo />
        <div className="logo-icon-wrapper">
          <Link href={"/dashboard/default"}>
            <Image
              width={35}
              height={35}
              className="img-fluid"
              src={`/assets/images/logo/logo-icon.png`}
              alt=""
            />
          </Link>
        </div>
        <SideBarMenu />
      </div>
    </div>
  );
};
