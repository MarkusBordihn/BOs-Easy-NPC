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

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.model.ModelType;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import java.lang.reflect.Proxy;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import net.minecraft.SharedConstants;
import net.minecraft.client.model.geom.ModelPart;
import net.minecraft.server.Bootstrap;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class EasyNPCModelManagerTest {

  private final EnumMap<ModelPartType, Boolean> modelPartVisibility =
      new EnumMap<>(ModelPartType.class);
  private ModelPart headPart;
  private ModelPart hatPart;
  private EasyNPCModelManager modelManager;

  @BeforeAll
  static void bootstrap() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
  }

  private static ModelPart createModelPart() {
    return new ModelPart(List.of(), Map.of());
  }

  private ModelDataCapable<?> createSyncOnlyModelData() {
    return (ModelDataCapable<?>)
        Proxy.newProxyInstance(
            ModelDataCapable.class.getClassLoader(),
            new Class<?>[] {ModelDataCapable.class},
            (proxy, method, arguments) ->
                switch (method.getName()) {
                  case "getModelPose" -> ModelPose.CUSTOM;
                  case "getModelType" -> ModelType.HUMANOID;
                  case "getModelPartVisibility" -> this.modelPartVisibility;
                  default -> throw new UnsupportedOperationException(method.getName());
                });
  }

  @BeforeEach
  void setUp() {
    this.headPart = createModelPart();
    this.hatPart = createModelPart();
    this.modelManager = new EasyNPCModelManager(createModelPart(), resourceLocation -> null);
    this.modelManager.defineModelPart(ModelPartType.HEAD, this.headPart);
    this.modelManager.defineModelPart(ModelPartType.HAT, this.hatPart);
  }

  @Test
  @DisplayName("Hat is hidden together with a hidden head")
  void hidesHatWithHiddenHead() {
    this.headPart.visible = false;

    this.modelManager.syncModelParts(this.createSyncOnlyModelData());

    assertFalse(this.hatPart.visible);
  }

  @Test
  @DisplayName("Hat is shown again once the head is visible")
  void showsHatWithVisibleHead() {
    this.hatPart.visible = false;

    this.modelManager.syncModelParts(this.createSyncOnlyModelData());

    assertTrue(this.hatPart.visible);
  }

  @Test
  @DisplayName("Explicitly hidden hat stays hidden with a visible head")
  void keepsExplicitlyHiddenHat() {
    this.modelPartVisibility.put(ModelPartType.HAT, false);

    this.modelManager.syncModelParts(this.createSyncOnlyModelData());

    assertFalse(this.hatPart.visible);
  }
}
