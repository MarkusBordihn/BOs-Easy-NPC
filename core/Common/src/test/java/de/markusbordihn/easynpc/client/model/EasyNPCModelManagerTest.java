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
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.data.model.ModelType;
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.data.scale.CustomScale;
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
  private final EnumMap<ModelPartType, CustomRotation> modelPartRotation =
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
                  case "getModelPartRotation" ->
                      this.modelPartRotation.getOrDefault(
                          (ModelPartType) arguments[0], new CustomRotation(0, 0, 0));
                  case "getModelPartPosition" -> new CustomPosition(0, 0, 0);
                  case "getModelPartScale" -> new CustomScale(1, 1, 1);
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

  @Test
  @DisplayName("Reset shows an outer layer again that the previous NPC hid")
  void resetShowsOuterLayerHiddenByPreviousNPC() {
    this.hatPart.visible = false;

    this.modelManager.resetModelParts();

    assertTrue(this.hatPart.visible);
  }

  @Test
  @DisplayName("Reset keeps an outer layer hidden that the model hides by default")
  void resetKeepsOuterLayerHiddenByModel() {
    ModelPart sleevePart = createModelPart();
    sleevePart.visible = false;
    this.modelManager.defineModelPart(ModelPartType.LEFT_SLEEVE, sleevePart);
    sleevePart.visible = true;

    this.modelManager.resetModelParts();

    assertFalse(sleevePart.visible);
  }

  @Test
  @DisplayName("Reset restores the pose the previous NPC left on a part")
  void resetRestoresPoseOfPreviousNPC() {
    this.headPart.setPos(1.0F, -2.0F, 3.0F);
    this.headPart.setRotation(0.6F, 0.4F, 0.2F);
    this.headPart.xScale = 1.5F;
    this.headPart.yScale = 1.5F;
    this.headPart.zScale = 1.5F;
    this.headPart.visible = false;

    this.modelManager.resetModelParts();

    assertEquals(0.0F, this.headPart.x);
    assertEquals(0.0F, this.headPart.y);
    assertEquals(0.0F, this.headPart.z);
    assertEquals(0.0F, this.headPart.xRot);
    assertEquals(0.0F, this.headPart.yRot);
    assertEquals(0.0F, this.headPart.zRot);
    assertEquals(1.0F, this.headPart.xScale);
    assertEquals(1.0F, this.headPart.yScale);
    assertEquals(1.0F, this.headPart.zScale);
    assertTrue(this.headPart.visible);
  }

  @Test
  @DisplayName("Custom pose wins over a part a model subclass overwrote")
  void reapplyRestoresChangedPartOverwrittenBySubclass() {
    this.modelPartRotation.put(ModelPartType.HEAD, new CustomRotation(0.6F, 0.0F, 0.0F));
    this.headPart.xRot = -1.4F;

    this.modelManager.reapplyChangedModelParts(this.createSyncOnlyModelData());

    assertEquals(0.6F, this.headPart.xRot);
  }

  @Test
  @DisplayName("Parts without custom pose keep the model subclass animation")
  void reapplyKeepsSubclassAnimationOfUnchangedPart() {
    ModelPart rightArmPart = createModelPart();
    this.modelManager.defineModelPart(ModelPartType.RIGHT_ARM, rightArmPart);
    rightArmPart.xRot = -1.4F;

    this.modelManager.reapplyChangedModelParts(this.createSyncOnlyModelData());

    assertEquals(-1.4F, rightArmPart.xRot);
  }

  @Test
  @DisplayName("Hidden head stays hidden when a model subclass shows it again")
  void reapplyKeepsHiddenPartHidden() {
    this.modelPartVisibility.put(ModelPartType.HEAD, false);
    this.headPart.visible = true;

    this.modelManager.reapplyChangedModelParts(this.createSyncOnlyModelData());

    assertFalse(this.headPart.visible);
    assertFalse(this.hatPart.visible);
  }
}
