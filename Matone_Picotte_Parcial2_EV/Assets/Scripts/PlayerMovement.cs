using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class PlayerMovement
{
    Transform _transform;
    Rigidbody _rb;
    float _movSpeed;

    public PlayerMovement(Transform t, Rigidbody rb, float speed)
    {
        _transform = t;
        _rb = rb;
        _movSpeed = speed;
    }

    public void Move(Vector3 inputVector)
    {
        if (inputVector.x != 0 || inputVector.z != 0)
        {
            Vector3 _dir = _transform.right * inputVector.x + _transform.forward * inputVector.z;
            //Vector3 _dir = new Vector3(inputVector.x, 0, inputVector.z).normalized;
            _rb.MovePosition(_rb.position + _dir * _movSpeed * Time.deltaTime);
        }
    }
}
