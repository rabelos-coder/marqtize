"use client";

import Image from "next/image";
import { useTranslations } from "next-intl";
import { useEffect, useState } from "react";
import { FaChevronDown } from "react-icons/fa";
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

export const NavBar = () => {
  const pathname = usePathname();

  const [isOpen, setIsOpen] = useState(false);
  const [disabled, setDisabled] = useState(false);
  const [isOpenLogin, setIsOpenLogin] = useState(false);
  const [loggedIn, setLoggedIn] = useState(false);
  const [color, setColor] = useState("");

  const dispatch = useAppDispatch();
  const { isLoggedIn } = useAppSelector((state) => state.auth);

  const toggle = () => setIsOpen(!isOpen);
  const toggleLogin = () => setIsOpenLogin(!isOpenLogin);

  const t = useTranslations("translations");

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
    setColor("");
    window.addEventListener("scroll", changeColor);
    setLoggedIn(isLoggedIn);

    return function () {
      window.removeEventListener("scroll", changeColor);
    };
  }, [isLoggedIn, pathname]);

  return (
    <>
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
              <NavItem>
                <NavLink href="/about-us" tag={Link}>
                  {t("aboutUs")}
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
                          "url(/assets/images/theme/landing/bg-dropdown-xl.jpg)",
                      }}
                    >
                      <div className="d-flex h-100 w-100 align-items-center justify-content-center">
                        <div className="text-white text-center z-1">
                          <div className="mb-3">
                            Multipurpose landing pages for a variety of
                            projects.
                          </div>
                          <Link
                            className="btn btn-white btn-sm text-primary fw-500"
                            href="index.html"
                          >
                            View All
                          </Link>
                        </div>
                      </div>
                    </Col>
                    <Col lg={7} className="p-lg-5">
                      <Row>
                        <Col lg={6}>
                          <h6 className="dropdown-header text-primary">
                            Applications
                          </h6>
                          <DropdownItem
                            href="landing-app-mobile.html"
                            tag={Link}
                          >
                            Mobile App
                          </DropdownItem>
                          <DropdownItem
                            tag={Link}
                            href="landing-app-desktop.html"
                          >
                            Desktop App
                          </DropdownItem>
                          <DropdownItem
                            divider
                            className="border-0 d-lg-none"
                          />
                          <h6 className="dropdown-header text-primary">
                            Business
                          </h6>
                          <DropdownItem
                            tag={Link}
                            href="landing-multipurpose.html"
                          >
                            Multipurpose
                          </DropdownItem>
                          <DropdownItem tag={Link} href="landing-agency.html">
                            Agency
                          </DropdownItem>
                          <DropdownItem tag={Link} href="landing-press.html">
                            Press
                          </DropdownItem>
                          <DropdownItem
                            tag={Link}
                            href="landing-directory.html"
                          >
                            Directory
                          </DropdownItem>
                          <DropdownItem tag={Link} href="landing-rental.html">
                            Rental
                          </DropdownItem>
                          <DropdownItem
                            tag={Link}
                            href="landing-real-estate.html"
                          >
                            Real Estate
                          </DropdownItem>
                          <DropdownItem
                            tag={Link}
                            href="landing-classifieds.html"
                          >
                            Classifieds
                          </DropdownItem>
                          <DropdownItem
                            divider
                            className="border-0 d-lg-none"
                          />
                          <h6 className="dropdown-header text-primary">
                            Lead Generation
                          </h6>
                          <DropdownItem
                            tag={Link}
                            href="landing-lead-capture.html"
                          >
                            Lead Capture
                          </DropdownItem>
                          <div className="dropdown-divider border-0 d-lg-none"></div>
                        </Col>
                        <Col lg={6}>
                          <h6 className="dropdown-header text-primary">
                            Personal
                          </h6>
                          <DropdownItem tag={Link} href="landing-resume.html">
                            Resume
                          </DropdownItem>
                          <DropdownItem
                            tag={Link}
                            href="landing-portfolio.html"
                          >
                            Portfolio
                          </DropdownItem>
                          <DropdownItem
                            divider
                            className="border-0 d-lg-none"
                          />
                          <h6 className="dropdown-header text-primary">
                            Header Styles
                          </h6>
                          <DropdownItem tag={Link} href="header-basic.html">
                            Basic
                          </DropdownItem>
                          <DropdownItem
                            tag={Link}
                            href="header-basic-signup.html"
                          >
                            Basic (Signup)
                          </DropdownItem>
                          <DropdownItem tag={Link} href="header-graphic.html">
                            Graphic
                          </DropdownItem>
                          <DropdownItem
                            tag={Link}
                            href="header-graphic-signup.html"
                          >
                            Graphic (Signup)
                          </DropdownItem>
                          <DropdownItem tag={Link} href="header-video.html">
                            Video Header
                            <span className="badge bg-primary-soft text-primary ms-1">
                              New!
                            </span>
                          </DropdownItem>
                          <DropdownItem
                            tag={Link}
                            href="header-inner-page.html"
                          >
                            Inner Page
                          </DropdownItem>
                          <DropdownItem tag={Link} href="header-nav-only.html">
                            Nav Only
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
                className="dropdown-xl no-caret"
              >
                <DropdownToggle nav className="dropdown-toggle">
                  Pages
                  <FaChevronDown className="dropdown-arrow" />
                </DropdownToggle>
                <DropdownMenu className="dropdown-menu-end me-lg-n20 me-xl-n15 animated--fade-in-up">
                  <Row className="g-0">
                    <div className="col-lg-4 p-lg-5">
                      <h6 className="dropdown-header text-primary">Company</h6>
                      <DropdownItem tag={Link} href="page-basic.html">
                        Basic Page
                      </DropdownItem>
                      <DropdownItem tag={Link} href="page-company-about.html">
                        About
                      </DropdownItem>
                      <DropdownItem tag={Link} href="page-company-pricing.html">
                        Pricing
                      </DropdownItem>
                      <DropdownItem tag={Link} href="page-company-contact.html">
                        Contact
                      </DropdownItem>
                      <DropdownItem tag={Link} href="page-company-team.html">
                        Team
                      </DropdownItem>
                      <DropdownItem tag={Link} href="page-company-terms.html">
                        Terms
                      </DropdownItem>
                      <DropdownItem divider className="border-0 d-lg-none" />
                      <h6 className="dropdown-header text-primary">Support</h6>
                      <DropdownItem tag={Link} href="page-help-center.html">
                        Help Center
                      </DropdownItem>
                      <DropdownItem
                        tag={Link}
                        href="page-help-knowledgebase.html"
                      >
                        Knowledgebase
                      </DropdownItem>
                      <DropdownItem
                        tag={Link}
                        href="page-help-message-center.html"
                      >
                        Message Center
                      </DropdownItem>
                      <DropdownItem
                        tag={Link}
                        href="page-help-support-ticket.html"
                      >
                        Support Ticket
                      </DropdownItem>
                      <div className="dropdown-divider border-0 d-lg-none"></div>
                    </div>
                    <div className="col-lg-4 p-lg-5">
                      <h6 className="dropdown-header text-primary">Careers</h6>
                      <DropdownItem
                        tag={Link}
                        href="page-careers-overview.html"
                      >
                        Careers List
                      </DropdownItem>
                      <DropdownItem tag={Link} href="page-careers-listing.html">
                        Position Details
                      </DropdownItem>
                      <DropdownItem divider className="border-0 d-lg-none" />
                      <h6 className="dropdown-header text-primary">Blog</h6>
                      <DropdownItem tag={Link} href="page-blog-overview.html">
                        Overview
                      </DropdownItem>
                      <DropdownItem tag={Link} href="page-blog-post.html">
                        Post
                      </DropdownItem>
                      <DropdownItem tag={Link} href="page-blog-archive.html">
                        Archive
                      </DropdownItem>
                      <DropdownItem divider className="border-0 d-lg-none" />
                      <h6 className="dropdown-header text-primary">
                        Portfolio
                      </h6>
                      <DropdownItem tag={Link} href="page-portfolio-grid.html">
                        Grid
                      </DropdownItem>
                      <DropdownItem
                        tag={Link}
                        href="page-portfolio-large-grid.html"
                      >
                        Large Grid
                      </DropdownItem>
                      <DropdownItem
                        tag={Link}
                        href="page-portfolio-masonry.html"
                      >
                        Masonry
                      </DropdownItem>
                      <DropdownItem
                        tag={Link}
                        href="page-portfolio-case-study.html"
                      >
                        Case Study
                      </DropdownItem>
                      <DropdownItem
                        tag={Link}
                        href="page-portfolio-project.html"
                      >
                        Project
                      </DropdownItem>
                      <DropdownItem divider className="border-0 d-lg-none" />
                    </div>
                    <div className="col-lg-4 p-lg-5">
                      <h6 className="dropdown-header text-primary">Error</h6>
                      <DropdownItem tag={Link} href="page-error-400.html">
                        400 Error
                      </DropdownItem>
                      <DropdownItem tag={Link} href="page-error-401.html">
                        401 Error
                      </DropdownItem>
                      <DropdownItem tag={Link} href="page-error-404-1.html">
                        404 Error (Option 1)
                      </DropdownItem>
                      <DropdownItem tag={Link} href="page-error-404-2.html">
                        404 Error (Option 2)
                      </DropdownItem>
                      <DropdownItem tag={Link} href="page-error-500.html">
                        500 Error
                      </DropdownItem>
                      <DropdownItem tag={Link} href="page-error-503.html">
                        503 Error
                      </DropdownItem>
                      <DropdownItem tag={Link} href="page-error-504.html">
                        504 Error
                      </DropdownItem>
                    </div>
                  </Row>
                </DropdownMenu>
              </UncontrolledDropdown>
              <UncontrolledDropdown nav inNavbar className="no-caret">
                <DropdownToggle nav className="dropdown-toggle">
                  Documentation
                  <FaChevronDown className="dropdown-arrow" />
                </DropdownToggle>
                <DropdownMenu className="dropdown-menu-end animated--fade-in-up">
                  <DropdownItem
                    className="py-3"
                    href="https://docs.startbootstrap.com/sb-ui-kit-pro/quickstart"
                    tag={Link}
                  >
                    <div className="icon-stack bg-primary-soft text-primary me-4">
                      <FiBookOpen width={24} height={24} />
                    </div>
                    <div>
                      <div className="small text-gray-500">Documentation</div>
                      Usage instructions and reference
                    </div>
                  </DropdownItem>
                  <DropdownItem divider className="m-0" />
                  <DropdownItem
                    className="py-3"
                    href="https://docs.startbootstrap.com/sb-ui-kit-pro/components"
                    tag={Link}
                  >
                    <div className="icon-stack bg-primary-soft text-primary me-4">
                      <FiCode width={24} height={24} />
                    </div>
                    <div>
                      <div className="small text-gray-500">Components</div>
                      Code snippets and reference
                    </div>
                  </DropdownItem>
                  <DropdownItem divider className="m-0" />
                  <DropdownItem
                    className="py-3"
                    href="https://docs.startbootstrap.com/sb-ui-kit-pro/changelog"
                    tag={Link}
                  >
                    <div className="icon-stack bg-primary-soft text-primary me-4">
                      <FiFileText width={24} height={24} />
                    </div>
                    <div>
                      <div className="small text-gray-500">Changelog</div>
                      Updates and changes
                    </div>
                  </DropdownItem>
                </DropdownMenu>
              </UncontrolledDropdown>
              <NavItem>
                <NavLink href="/contact" tag={Link}>
                  {t("contact")}
                </NavLink>
              </NavItem>
            </Nav>
            <Nav className="ml-auto" navbar>
              {loggedIn ? (
                <>
                  <NavItem>
                    <NavLink href="/backend" tag={Link}>
                      {t("dashboard")}
                    </NavLink>
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
                  <Button
                    type="button"
                    color="primary"
                    className="text-uppercase"
                    onClick={toggleLogin}
                  >
                    {t("signIn")}
                  </Button>
                </NavItem>
              )}
            </Nav>
          </Collapse>
        </Container>
      </Navbar>
    </>
  );
};
