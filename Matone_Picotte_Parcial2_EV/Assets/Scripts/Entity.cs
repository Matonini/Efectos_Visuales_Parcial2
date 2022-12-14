using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public abstract class Entity : MonoBehaviour
{
    [Header ("Variables de Vida")]
    [SerializeField]
    protected int   _currHp;
    [SerializeField]
    protected int   _maxHp;

    public int Health
    {
        get
        {
            return _currHp;
        }
    }
    public int MaxHealth
    {
        get
        {
            return _maxHp;
        }
    }


    public virtual void IncreaseHealth(int value)
    {
        _currHp += value;
        if (_currHp > _maxHp)
        {
            _currHp = _maxHp;
        }
    }

    public virtual void DecreaseHealth(int value)
    {
        _currHp -= value;
        if (_currHp <= 0)
        {
            _currHp = 0;
            Death();
        }
    }

    public virtual void Death()
    {
        gameObject.SetActive(false);
        //play sounds etc
    }
}
