#!/usr/bin/python3

from gssa.comparator import Comparator
from collections import Counter
from lxml.etree import XMLSyntaxError
import pytest


def test_comparator_trivial_fails():
    with pytest.raises(XMLSyntaxError):
        Comparator("", "")


def test_comparator_trivial_xml_succeeds():
    comparator = Comparator("<simulationDefinition/>", "<simulationDefinition/>")
    assert comparator.equal()


def test_comparator_transferrer_only_succeeds():
    both = """
      <simulationDefinition>
        <transferrer class="http">
          <url>http://example.com</url>
        </transferrer>
      </simulationDefinition>
    """
    comparator = Comparator(both, both)
    assert comparator.equal()


def test_comparator_transferrer_only_fails():
    left = """
      <simulationDefinition>
        <transferrer class="http">
          <url>http://example.com</url>
        </transferrer>
      </simulationDefinition>
    """
    right = """
      <simulationDefinition>
        <transferrer class="ftp">
          <url>ftp://example.com</url>
        </transferrer>
      </simulationDefinition>
    """
    comparator = Comparator(left, right)
    messages = [
        "Transferrer: classes differ - http // ftp",
        "Transferrer: URLs differ - http://example.com // ftp://example.com"
    ]
    assert Counter(messages) == Counter(comparator.diff())


def test_comparator_numerical_model_needles_only_succeeds():
    left = """
      <simulationDefinition>
        <numericalModel>
          <needles>
            <needle index='12388' class='solid-boundary' file='library:cryo-two-part-cylinder-1'>
              <parameters>
                <parameter name="NEEDLE_TIP_LOCATION" value="[-1, 0.3, 1.2]" type="array(float)"/>
                <parameter name="NEEDLE_ENTRY_LOCATION" value="[-1, 0.5, 1.2]" type="array(float)"/>
              </parameters>
            </needle>
          </needles>
        </numericalModel>
      </simulationDefinition>
    """
    right = """
      <simulationDefinition>
        <numericalModel>
          <needles>
            <needle index='12388' class='solid-boundary' file='library:cryo-two-part-cylinder-1'>
              <parameters>
                <parameter name="NEEDLE_TIP_LOCATION" value="[-1, 0.3, 1.2]" type="array(float)"/>
                <parameter name="NEEDLE_ENTRY_LOCATION" value="[-1, 0.5, 1.2]" type="array(float)"/>
              </parameters>
            </needle>
          </needles>
        </numericalModel>
      </simulationDefinition>
    """
    comparator = Comparator(left, right)
    assert comparator.equal()


def test_comparator_numerical_model_needles_only_fails():
    left = """
      <simulationDefinition>
        <numericalModel>
          <needles>
            <needle index='12398' class='boundary' file='library:cryo-two-part-cylinder'>
              <parameters>
                <parameter name="NEEDLE_TIP_LOCATION" value="[-1,0.3, 1.2]" type="array(float)"/>
                <parameter name="NEEDLE_ENTRY_LOC" value="[-1, 0.5, 1.2]" type="array(float)"/>
              </parameters>
            </needle>
            <needle index='12388' class='solid-boundary' file='library:cryo-two-part-cylinder-1'>
              <parameters>
                <parameter name="NEEDLE_TIP_LOCATION" value="[-1, 0.3, 1.2]" type="array(float)"/>
                <parameter name="NEEDLE_ENTRY_LOCATION" value="[-1, 0.5, 1.2]" type="array(float)"/>
                <parameter name="NEEDLE_TEMPERATURE" value="100" type="float"/>
              </parameters>
            </needle>
          </needles>
        </numericalModel>
      </simulationDefinition>
    """
    right = """
      <simulationDefinition>
        <numericalModel>
          <needles>
            <needle index='12388' class='boundary' file='library:cryo-two-part-cylinder'>
              <parameters>
                <parameter name="NEEDLE_TIP_LOCATION" value="[-1,0.3, 1.2]" type="array(float)"/>
                <parameter name="NEEDLE_ENTRY_LOC" value="[-1, 0.5, 1.2]" type="array(float)"/>
                <parameter name="NEEDLE_TEMPERATURE" value="373" type="float"/>
              </parameters>
            </needle>
          </needles>
        </numericalModel>
      </simulationDefinition>
    """
    comparator = Comparator(left, right)
    messages = [
        'Needle: this (12398) has no parameter NEEDLE_TEMPERATURE',
        'Numerical Model: this has different needle count than that'
    ]
    assert Counter(messages) == Counter(comparator.diff())


def test_comparator_numerical_model_regions_only_succeeds():
    left = """
      <simulationDefinition>
        <numericalModel>
          <regions>
            <region id='organ-0' name='organ' format="surface" input="something/kidney.vtp" groups='[&quot;boundary&quot;]'/>
          </regions>
        </numericalModel>
      </simulationDefinition>
    """
    right = """
      <simulationDefinition>
        <numericalModel>
          <regions>
            <region id='organ-0' name='organ' format="surface" input="something/kidney.vtp" groups='[&quot;boundary&quot;]'/>
          </regions>
        </numericalModel>
      </simulationDefinition>
    """
    comparator = Comparator(left, right)
    assert comparator.equal()


def test_comparator_numerical_model_regions_only_fails():
    left = """
      <simulationDefinition>
        <numericalModel>
          <regions>
            <region id='organ-0' name='organ' format="surface" input="something/kidney.vtp" groups='[&quot;boundary&quot;]'/>
          </regions>
        </numericalModel>
      </simulationDefinition>
    """
    right = """
      <simulationDefinition>
        <numericalModel>
          <regions>
            <region id='organ-0' name="organ" format='surface' input="something/kidney.vtk" groups='[&quot;boundary&quot;, &quot;no-flux&quot;]'/>
            <region id='organ-1' name="organ" format='surface' input="something/kidney.vtk" groups='[&quot;boundary&quot;, &quot;no-flux&quot;]'/>
          </regions>
        </numericalModel>
      </simulationDefinition>
    """
    comparator = Comparator(left, right)
    messages = [
        'Numerical Model: this has no region organ-1',
        'Region: for ID organ-0, input fields differ something/kidney.vtp // something/kidney.vtk',
        'Region: this (organ-0) has no group no-flux'
    ]
    assert Counter(messages) == Counter(comparator.diff())


def test_comparator_numerical_models_only_succeeds():
    left = """
      <simulationDefinition>
        <numericalModel>
          <needles/>
          <regions/>
          <definition>
           A Definition
          </definition>
        </numericalModel>
      </simulationDefinition>
    """
    right = """
      <simulationDefinition>
        <numericalModel>
          <regions/>
          <needles/>
          <definition>
           A Definition
          </definition>
        </numericalModel>
      </simulationDefinition>
    """
    comparator = Comparator(left, right)
    assert comparator.equal()


def test_comparator_numerical_models_only_fails():
    left = """
      <simulationDefinition>
        <numericalModel>
          <definition>
           A Definition
          </definition>
        </numericalModel>
      </simulationDefinition>
    """
    right = """
      <simulationDefinition>
        <numericalModel>
          <regions/>
          <needles/>
        </numericalModel>
      </simulationDefinition>
    """
    comparator = Comparator(left, right)
    messages = [
        "Numerical Model: that has no definition",
    ]
    assert Counter(messages) == Counter(comparator.diff())


def test_comparator_parameters_only_succeeds():
    left = """
      <simulationDefinition>
        <parameters>
          <parameter name="BANANA" value="5.0" type="float"/>
          <parameter name="PEAR" value="3" type="int"/>
        </parameters>
      </simulationDefinition>
    """
    right = """
      <simulationDefinition>
        <parameters>
          <parameter name="PEAR" value="3" type="int"/>
          <parameter name="BANANA" value="5.00" type="float"/>
        </parameters>
      </simulationDefinition>
    """
    comparator = Comparator(left, right)
    assert comparator.equal()


def test_comparator_parameters_only_fails():
    left = """
      <simulationDefinition>
        <parameters>
          <parameter name="BANANA" value="5.0" type="float"/>
          <parameter name="PEAR" value="3" type="float"/>
        </parameters>
      </simulationDefinition>
    """
    right = """
      <simulationDefinition>
        <parameters>
          <parameter name="PEAR" value="3" type="int"/>
          <parameter name="BANANA" value="5.01" type="float"/>
        </parameters>
      </simulationDefinition>
    """
    comparator = Comparator(left, right)
    messages = [
        "Parameter BANANA: values differ - 5.0 // 5.01",
        "Parameter PEAR: types differ - float // int"
    ]
    assert Counter(messages) == Counter(comparator.diff())


def test_comparator_algorithms_only_succeeds():
    left = """
      <simulationDefinition>
        <algorithms>
          <algorithm result="CONSTANT_KIWI">
            <arguments>
              <argument name="Time"  />
            </arguments>
            <content>
            </content>
          </algorithm>
          <algorithm result="CONSTANT_PASSIONFRUIT">
            <arguments>
              <argument name="Temperature"  />
              <argument name="Time"/>
            </arguments>
            <content>
              function CONSTANT_PASSIONFRUIT( Temperature, Time )
              {
                _CONSTANT_PASSIONFRUIT = Temperature * Time;
              }
            </content>
          </algorithm>
        </algorithms>
      </simulationDefinition>
    """
    right = """
      <simulationDefinition>
        <algorithms>
          <algorithm result="CONSTANT_KIWI">
            <arguments>
              <argument name="Time" />
            </arguments>
            <content>


            </content>
          </algorithm>
          <algorithm result="CONSTANT_PASSIONFRUIT">
            <arguments>
              <argument name="Temperature" />
              <argument name="Time" />
            </arguments>
            <content>
             function CONSTANT_PASSIONFRUIT( Temperature, Time )
              {
                _CONSTANT_PASSIONFRUIT = Temperature * Time;
              }

            </content>
          </algorithm>
        </algorithms>
      </simulationDefinition>
    """
    comparator = Comparator(left, right)
    assert comparator.equal()


def test_comparator_algorithms_only_fails():
    left = """
      <simulationDefinition>
        <algorithms>
          <algorithm result="CONSTANT_KIWI">
            <arguments>
              <argument name="Time"  />
            </arguments>
            <content>
            </content>
          </algorithm>
          <algorithm result="CONSTANT_PASSIONFRUIT">
            <arguments>
              <argument name="Temperature"  />
              <argument name="Time"/>
            </arguments>
            <content>
              function CONSTANT_PASSIONFRUIT( Temperature, Time )
              {
                _CONSTANT_PASSIONFRUIT = Temperature * Time;
              }
            </content>
          </algorithm>
        </algorithms>
      </simulationDefinition>
    """
    right = """
      <simulationDefinition>
        <algorithms>
          <algorithm result="CONSTANT_KIWI ">
            <arguments>
              <argument name="time"  />
              <argument name="Temperature"  />
            </arguments>
            <content>
            </content>
          </algorithm>
          <algorithm result="CONSTANT_PASSIONFRUIT">
            <arguments>
              <argument name="Time"/>
            </arguments>
            <content>
              function CONSTANT_PASSIONFRUIT( Temperature, Time )
              {
                _CONSTANT_PASSIONFRUIT = Temperature * Time;
              }
            </content>
          </algorithm>
        </algorithms>
      </simulationDefinition>
    """
    comparator = Comparator(left, right)
    messages = [
        "Algorithm: CONSTANT_PASSIONFRUIT has no argument Temperature",
        "Left definition has no algorithm 'CONSTANT_KIWI '",
        "Right definition has no algorithm 'CONSTANT_KIWI'"
    ]
    assert Counter(messages) == Counter(comparator.diff())
