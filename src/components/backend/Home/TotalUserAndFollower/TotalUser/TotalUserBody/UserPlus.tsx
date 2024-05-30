import { HiUserAdd } from '@react-icons/all-files/hi/HiUserAdd'

const UserPlus = () => {
  return (
    <li>
      <div className="user-icon primary">
        <div className="user-box">
          <HiUserAdd className="font-primary" size={24} />
        </div>
      </div>
      <div>
        <h5 className="mb-1">178,098</h5>
        <span className="font-primary d-flex align-items-center">
          <i className="icon-arrow-up icon-rotate me-1"> </i>
          <span className="f-w-500">+30.89</span>
        </span>
      </div>
    </li>
  )
}

export default UserPlus
