import Image from "next/image";

import { useCustomizer, useLayout } from "@/hooks";
import { Link } from "@/navigation";

import { SidebarLogo } from "./SidebarLogo";
import { SidebarMenu } from "./SidebarMenu";

export const SideBar = () => {
  const { sidebarIconType } = useCustomizer();
  const { sideBarToggle } = useLayout();

  return (
    <div
      className={`sidebar-wrapper ${sideBarToggle ? "close_icon" : ""}`}
      sidebar-layout={sidebarIconType}
    >
      <div>
        <SidebarLogo />
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
        <SidebarMenu />
      </div>
    </div>
  );
};
