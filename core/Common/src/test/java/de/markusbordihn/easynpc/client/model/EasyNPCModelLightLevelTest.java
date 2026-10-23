/*
 * Copyright 2026 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.client.model;

import static org.junit.jupiter.api.Assertions.assertEquals;

import de.markusbordihn.easynpc.data.display.DisplayAttributeDataSet;
import de.markusbordihn.easynpc.data.display.DisplayAttributeEntry;
import de.markusbordihn.easynpc.data.display.DisplayAttributeType;
import de.markusbordihn.easynpc.entity.easynpc.data.DisplayAttributeDataCapable;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Proxy;
import java.util.OptionalInt;
import net.minecraft.SharedConstants;
import net.minecraft.server.Bootstrap;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class EasyNPCModelLightLevelTest {

  @BeforeAll
  static void bootstrapMinecraft() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
  }

  private static DisplayAttributeDataCapable<?> createDisplayAttributeData(
      DisplayAttributeDataSet displayAttributeDataSet) {
    return (DisplayAttributeDataCapable<?>)
        Proxy.newProxyInstance(
            DisplayAttributeDataCapable.class.getClassLoader(),
            new Class<?>[] {DisplayAttributeDataCapable.class},
            (proxy, method, arguments) -> {
              if (method.getName().equals("getDisplayAttributeData")) {
                return displayAttributeDataSet;
              }
              if (method.isDefault()) {
                return InvocationHandler.invokeDefault(proxy, method, arguments);
              }

              throw new UnsupportedOperationException(method.getName());
            });
  }

  @Test
  @DisplayName("A new NPC uses the surrounding light level")
  void defaultLightLevelUsesSurroundingLight() {
    assertEquals(
        OptionalInt.empty(),
        EasyNPCModel.getFixedEntityLightLevel(
            createDisplayAttributeData(DisplayAttributeDataSet.createDefault())));
  }

  @ParameterizedTest
  @ValueSource(ints = {1, 7, 15})
  @DisplayName("A configured light level replaces the surrounding light level")
  void configuredLightLevelIsFixed(int lightLevel) {
    DisplayAttributeDataSet displayAttributeDataSet =
        DisplayAttributeDataSet.createDefault()
            .withAttribute(DisplayAttributeType.LIGHT_LEVEL, new DisplayAttributeEntry(lightLevel));

    assertEquals(
        OptionalInt.of(lightLevel),
        EasyNPCModel.getFixedEntityLightLevel(createDisplayAttributeData(displayAttributeDataSet)));
  }

  @Test
  @DisplayName("Missing display attribute data uses the surrounding light level")
  void missingDisplayAttributeDataUsesSurroundingLight() {
    assertEquals(OptionalInt.empty(), EasyNPCModel.getFixedEntityLightLevel(null));
  }
}
