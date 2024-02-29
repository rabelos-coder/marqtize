import Image from "next/image";

import { useCustomizer, useLayout } from "@/hooks";
import { Link } from "@/navigation";

import { SideBarLogo } from "./SidebarLogo";
import { SideBarMenu } from "./SidebarMenu";

export const SideBar = () => {
  const { sidebarIconType } = useCustomizer();
  const { sideBarToggle } = useLayout();

  return (
    <div
      className={`sidebar-wrapper ${sideBarToggle ? "close_icon" : ""}`}
      sidebar-layout={sidebarIconType}
    >
      <div>
        <SideBarLogo />
        <div className="logo-icon-wrapper">
          <Link href={"/backend"}>
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
