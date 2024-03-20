"use client";

import Image from "next/image";
import { useTranslations } from "next-intl";
import { useEffect, useState } from "react";
import { FaChevronCircleRight, FaChevronDown } from "react-icons/fa";
import { FiBookOpen, FiCode, FiFileText, FiMenu } from "react-icons/fi";
import { toast } from "react-toastify";
import {
  Button,
  Col,
  Collapse,
  Container,
  DropdownItem,
  DropdownMenu,
  DropdownToggle,
  Nav,
  Navbar,
  NavbarBrand,
  NavItem,
  NavLink,
  Row,
  UncontrolledDropdown,
} from "reactstrap";
import Sawl from "sweetalert2";

import { APP_META_TITLE } from "@/environment";
import { useAppDispatch, useAppSelector } from "@/hooks";
import { Link, usePathname } from "@/navigation";
import { resetAuth } from "@/store/slices/authSlice";

type NavbarProps = {
  navbarExpanded?: boolean;
};

export const NavBar = ({ navbarExpanded }: NavbarProps) => {
  const pathname = usePathname();

  const [isOpen, setIsOpen] = useState(false);
  const [disabled, setDisabled] = useState(false);
  const [loggedIn, setLoggedIn] = useState(false);
  const [color, setColor] = useState(navbarExpanded ? "navbar-scrolled" : "");

  const dispatch = useAppDispatch();
  const { isLoggedIn } = useAppSelector((state) => state.auth);

  const toggle = () => setIsOpen(!isOpen);

  const t = useTranslations();

  const logoutConfirm = () => {
    setDisabled(true);
    Sawl.fire({
      title: t("confirmation"),
      text: t("logoutConfirm"),
      icon: "question",
      showCancelButton: true,
      confirmButtonText: t("yes"),
      cancelButtonText: t("no"),
    }).then(({ isConfirmed }) => {
      if (isConfirmed) {
        dispatch(resetAuth());
        setLoggedIn(false);
        toast.success(t("logoutSuccess"));
      }
    });

    setDisabled(false);
  };

  const changeColor = () => {
    if (
      document.documentElement.scrollTop > 99 ||
      document.body.scrollTop > 99
    ) {
      setColor("navbar-scrolled");
    } else if (
      document.documentElement.scrollTop < 100 ||
      document.body.scrollTop < 100
    ) {
      setColor("");
    }
  };

  useEffect(() => {
    if (!navbarExpanded) window.addEventListener("scroll", changeColor);
    setLoggedIn(isLoggedIn);

    return function () {
      window.removeEventListener("scroll", changeColor);
    };
  }, [isLoggedIn, pathname, navbarExpanded]);

  return (
    <div className={`${navbarExpanded ? "navbar-fixed-top" : ""}`}>
      <Navbar
        className={`navbar-marketing bg-transparent navbar-dark ${color}`}
        fixed="top"
        expand="lg"
        container={false}
      >
        <Container className="px-5">
          <NavbarBrand className="logo text-white" href="/" tag={Link}>
            <Image
              src="/assets/images/logo/marqtize_logo_solo.png"
              width={30}
              height={30}
              alt={APP_META_TITLE}
              className="img-fluid me-2"
            />
            <span>{APP_META_TITLE}</span>
          </NavbarBrand>
          <button
            onClick={toggle}
            className="navbar-toggler collapsed"
            aria-expanded={isOpen}
            aria-label="Toggle navigation"
          >
            <FiMenu width={24} height={24} />
          </button>
          <Collapse navbar isOpen={isOpen}>
            <Nav navbar className="ms-auto me-lg-5">
              <NavItem>
                <NavLink href="/" tag={Link}>
                  {t("home")}
                </NavLink>
              </NavItem>
              <UncontrolledDropdown
                nav
                inNavbar
                className="dropdown-xl no-caret"
              >
                <DropdownToggle nav className="dropdown-toggle">
                  {t("products")}
                  <FaChevronDown className="dropdown-arrow" />
                </DropdownToggle>
                <DropdownMenu className="dropdown-menu-end animated--fade-in-up me-lg-n25 me-xl-n15">
                  <Row className="g-0">
                    <Col
                      lg={5}
                      className="p-lg-3 bg-img-cover overlay overlay-primary overlay-70 d-none d-lg-block"
                      style={{
                        backgroundImage:
                          "url(/assets/images/themes/landing/bg-dropdown-xl.jpg)",
                      }}
                    >
                      <div className="d-flex h-100 w-100 align-items-center justify-content-center">
                        <div className="text-white z-1">
                          <h3 className="mb-3 text-white text-normal">
                            {t("knowHowToWorkOurPlatform")}
                          </h3>
                          <div className="mb-3 text-normal">
                            {t("exploreOurPlatform")}
                          </div>
                          <Button
                            color="white"
                            size="sm"
                            className="text-primary fw-500 text-start"
                            href="/platform-overview"
                            tag={Link}
                          >
                            {t("getAnOverview")}
                            <FaChevronCircleRight
                              width={24}
                              height={24}
                              className="ms-2"
                            />
                          </Button>
                        </div>
                      </div>
                    </Col>
                    <Col lg={7} className="p-lg-5">
                      <Row>
                        <Col lg={12}>
                          <h6 className="dropdown-header text-primary">
                            {t("applications")}
                          </h6>
                          <DropdownItem
                            tag={Link}
                            href="/products/ecommerce-platform"
                          >
                            {t("eCommercePlatform.title")}
                          </DropdownItem>
                          <DropdownItem
                            tag={Link}
                            href="/products/order-management-system"
                          >
                            {t("orderManagementSystem.title")}
                          </DropdownItem>
                          <DropdownItem
                            tag={Link}
                            href="/products/design-management-system"
                          >
                            {t("designManagementSystem.title")}
                          </DropdownItem>
                          <DropdownItem
                            tag={Link}
                            href="/products/marketplace-management-system"
                          >
                            {t("marketplaceManagementSystem.title")}
                          </DropdownItem>
                          <DropdownItem
                            tag={Link}
                            href="/products/sellers-management-system"
                          >
                            {t("sellersManagementSystem.title")}
                          </DropdownItem>
                        </Col>
                      </Row>
                    </Col>
                  </Row>
                </DropdownMenu>
              </UncontrolledDropdown>
              <UncontrolledDropdown
                nav
                inNavbar
                className="dropdown-lg no-caret"
              >
                <DropdownToggle nav className="dropdown-toggle">
                  {t("business")}
                  <FaChevronDown className="dropdown-arrow" />
                </DropdownToggle>
                <DropdownMenu className="dropdown-menu-end me-lg-n20 me-xl-n15 animated--fade-in-up">
                  <Row className="g-0">
                    <Col lg={5} className="p-lg-5">
                      <h6 className="dropdown-header text-primary">
                        {t("about")}
                      </h6>
                      <DropdownItem tag={Link} href="/about-us">
                        {t("aboutUs")}
                      </DropdownItem>
                      <DropdownItem tag={Link} href="/contact-us">
                        {t("contactUs")}
                      </DropdownItem>
                      <DropdownItem tag={Link} href="/support">
                        {t("helpCenter")}
                      </DropdownItem>
                    </Col>
                    <Col lg={7} className="p-lg-5">
                      <h6 className="dropdown-header text-primary">
                        {t("developers")}
                      </h6>
                      <DropdownItem tag={Link} href="/developers">
                        {t("developerPortal")}
                      </DropdownItem>
                    </Col>
                  </Row>
                </DropdownMenu>
              </UncontrolledDropdown>
              <UncontrolledDropdown nav inNavbar className="no-caret">
                <DropdownToggle nav className="dropdown-toggle">
                  {t("documentation.title")}
                  <FaChevronDown className="dropdown-arrow" />
                </DropdownToggle>
                <DropdownMenu className="dropdown-menu-end animated--fade-in-up">
                  <DropdownItem
                    className="py-3"
                    href="/documentation"
                    tag={Link}
                  >
                    <div className="icon-stack bg-primary-soft text-primary me-4">
                      <FiBookOpen width={24} height={24} />
                    </div>
                    <div>
                      <div className="small text-gray-500">
                        {t("documentation.title")}
                      </div>
                      {t("usageInstructions")}
                    </div>
                  </DropdownItem>
                  <DropdownItem divider className="m-0" />
                  <DropdownItem className="py-3" href="/components" tag={Link}>
                    <div className="icon-stack bg-primary-soft text-primary me-4">
                      <FiCode width={24} height={24} />
                    </div>
                    <div>
                      <div className="small text-gray-500">
                        {t("components.title")}
                      </div>
                      {t("codeSnippetsAndReference")}
                    </div>
                  </DropdownItem>
                  <DropdownItem divider className="m-0" />
                  <DropdownItem className="py-3" href="/changelog" tag={Link}>
                    <div className="icon-stack bg-primary-soft text-primary me-4">
                      <FiFileText width={24} height={24} />
                    </div>
                    <div>
                      <div className="small text-gray-500">
                        {t("changelog.title")}
                      </div>
                      {t("updatesAndChanges")}
                    </div>
                  </DropdownItem>
                </DropdownMenu>
              </UncontrolledDropdown>
              <NavItem>
                <NavLink href="/blog/1" tag={Link}>
                  {t("blog.title2")}
                </NavLink>
              </NavItem>
            </Nav>
            <Nav className="ml-auto" navbar>
              {loggedIn ? (
                <>
                  <NavItem>
                    <a className="nav-link" href="/backend">
                      {t("panel")}
                    </a>
                  </NavItem>
                  <NavItem className="d-none d-md-block">
                    <Button
                      type="button"
                      color="primary"
                      disabled={disabled}
                      className="ms-2 text-uppercase"
                      onClick={logoutConfirm}
                    >
                      {t("logout")}
                    </Button>
                  </NavItem>
                  <NavItem className="d-block d-md-none">
                    <NavLink
                      href="#"
                      className={`text-uppercase ${disabled ? "disabled" : ""}`}
                      onClick={(e) => {
                        if (!disabled) {
                          e.preventDefault();
                          logoutConfirm();
                        }
                      }}
                    >
                      {t("logout")}
                    </NavLink>
                  </NavItem>
                </>
              ) : (
                <NavItem>
                  <a
                    className="btn btn-primary text-uppercase"
                    href="/auth/login"
                  >
                    {t("signIn")}
                  </a>
                </NavItem>
              )}
            </Nav>
          </Collapse>
        </Container>
      </Navbar>
    </div>
  );
};
