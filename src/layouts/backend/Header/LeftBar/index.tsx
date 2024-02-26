import Image from "next/image";
import { Col } from "reactstrap";
import { AlignCenter } from "react-feather";
import { useLayout } from "@/hooks";
import { NotificationSlider } from "./NotificationSlider";
import { Link } from "@/navigation";

export const LeftBar = () => {
  const { sideBarToggle, setSideBarToggle } = useLayout();
  return (
    <>
      <Col className="header-logo-wrapper col-auto p-0">
        <div className="logo-wrapper">
          <Link href={"/backend"}>
            <Image
              className="img-fluid for-light"
              src={`/assets/images/logo/logo.png`}
              alt="logo"
              width={100}
              height={100}
            />
            <Image
              className="img-fluid for-dark"
              src={`/assets/images/logo/logo_dark.png`}
              alt="logo"
              width={100}
              height={100}
            />
          </Link>
        </div>
        <div
          className="toggle-sidebar"
          onClick={() => setSideBarToggle(!sideBarToggle)}
        >
          <AlignCenter
            className="status_toggle middle sidebar-toggle"
            id="sidebar-toggle"
          />
        </div>
      </Col>
      <Col xxl={5} xl={6} lg={5} md={4} sm={3} className="left-header p-0">
        <NotificationSlider />
      </Col>
    </>
  );
};
