using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Player : Entity
{
    public static Player instance;
    
    [Header("Fisicas y Variables de Movimiento")]
    public Rigidbody _myRB;
    public bool isGrounded;
    PlayerMovement _movement;
    Vector3 _InputVector;
    [SerializeField] bool _canMove;
    [SerializeField] float _movSpeed;

    private void Awake()
    {
        instance = this;
        _myRB = this.GetComponent<Rigidbody>();
    }

    private void Start()
    {
        _currHp = _maxHp;
        _movement = new PlayerMovement(transform, _myRB, _movSpeed);
    }

    private void Update()
    {
        _InputVector.x = Input.GetAxis("Horizontal");
        _InputVector.z = Input.GetAxis("Vertical");
        if(_canMove)
        {
            _movement.Move(_InputVector);
        }
    }
}
