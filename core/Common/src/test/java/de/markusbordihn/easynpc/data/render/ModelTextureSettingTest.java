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

package de.markusbordihn.easynpc.data.render;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Optional;
import net.minecraft.SharedConstants;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.Bootstrap;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

class ModelTextureSettingTest {

  private static final ResourceLocation TEXTURE = new ResourceLocation("my_pack", "textures/a.png");
  private static final ResourceLocation OTHER_TEXTURE =
      new ResourceLocation("my_pack", "textures/b.png");

  @BeforeAll
  static void bootstrap() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
  }

  @Test
  void roundTripsSlotsThroughTag() {
    ModelTextureSetting textureSetting =
        ModelTextureSetting.of(ModelTextureSetting.DEFAULT_SLOT, TEXTURE)
            .withSlot("#2", OTHER_TEXTURE, ModelTextureBlend.TRANSLUCENT)
            .withBlend("cape", ModelTextureBlend.TRANSLUCENT);

    assertEquals(textureSetting, ModelTextureSetting.fromTag(textureSetting.createTag()));
  }

  @Test
  void normalizesAndRejectsSlotNames() {
    assertEquals(Optional.of("head"), ModelTextureSetting.normalizeSlot("  HEAD "));
    assertEquals(Optional.of("#12"), ModelTextureSetting.normalizeSlot("#12"));
    assertEquals(Optional.empty(), ModelTextureSetting.normalizeSlot(""));
    assertEquals(Optional.empty(), ModelTextureSetting.normalizeSlot("#"));
    assertEquals(Optional.empty(), ModelTextureSetting.normalizeSlot("left arm"));
    assertEquals(Optional.empty(), ModelTextureSetting.normalizeSlot(null));

    ModelTextureSetting textureSetting = ModelTextureSetting.of("Head", TEXTURE);
    assertEquals(Optional.of(TEXTURE), textureSetting.texture("HEAD"));
    assertSame(textureSetting, textureSetting.withSlot("left arm", TEXTURE));
  }

  @Test
  void keepsBlendOnlySlotsAndDropsResetSlots() {
    ModelTextureSetting blendOnly =
        ModelTextureSetting.EMPTY.withBlend("head", ModelTextureBlend.TRANSLUCENT);

    assertFalse(blendOnly.isEmpty());
    assertEquals(Optional.empty(), blendOnly.texture("head"));
    assertEquals(ModelTextureBlend.TRANSLUCENT, blendOnly.blend("head"));
    assertTrue(blendOnly.withBlend("head", ModelTextureBlend.CUTOUT).isEmpty());
    assertTrue(blendOnly.withoutSlot("head").isEmpty());
  }

  @Test
  void writesTextureSettingOnlyWhenPresent() {
    RenderDataEntry renderData =
        new RenderDataEntry(RenderType.EASY_MODEL_ENTITY, null, "my_pack:my_model");

    assertFalse(renderData.createTag().contains(RenderDataEntry.DATA_RENDER_TEXTURE_SETTING_TAG));

    RenderDataEntry withTexture =
        renderData.withRenderTextureSetting(ModelTextureSetting.of("head", TEXTURE));
    assertEquals(
        withTexture.getRenderTextureSetting(),
        new RenderDataEntry(withTexture.createTag()).getRenderTextureSetting());
  }

  @Test
  void changingModelResetsTextureSetting() {
    RenderDataEntry renderData =
        new RenderDataEntry(RenderType.EASY_MODEL_ENTITY, null, "my_pack:my_model")
            .withRenderTextureSetting(ModelTextureSetting.of("head", TEXTURE));

    assertTrue(
        renderData
            .withRenderEntityModel("my_pack:other_model")
            .getRenderTextureSetting()
            .isEmpty());
  }
}
