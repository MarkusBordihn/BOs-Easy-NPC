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

package de.markusbordihn.easynpc.entity.easynpc.data;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;

import de.markusbordihn.easynpc.data.skin.variant.HumanoidSkinVariant;
import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.server.player.FakePlayer;
import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.goal.GoalSelector;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class VariantDataFallbackTest {

  @Test
  void testKnownVariantIsResolved() {
    TestVariantData variantData = new TestVariantData();

    assertSame(HumanoidSkinVariant.KNIGHT_01, variantData.getSkinVariantType("KNIGHT_01"));
  }

  @Test
  @DisplayName("A variant of another NPC type falls back instead of failing the entity")
  void testForeignVariantFallsBack() {
    TestVariantData variantData = new TestVariantData();

    assertSame(
        variantData.getDefaultSkinVariantType(), variantData.getSkinVariantType("SKELETON_01"));
    assertSame(variantData.getDefaultSkinVariantType(), variantData.getSkinVariantType(""));
    assertSame(variantData.getDefaultSkinVariantType(), variantData.getSkinVariantType(null));
  }

  @Test
  @DisplayName("An unknown variant keeps the entity on its default variant")
  void testUnknownVariantIsNotStored() {
    TestVariantData variantData = new TestVariantData();

    variantData.setSkinVariantType("DOES_NOT_EXIST");

    assertEquals(
        variantData.getDefaultSkinVariantType().name(),
        variantData.getSynchedEntityData(SynchedDataIndex.VARIANT_TYPE));
  }

  private static final class TestVariantData implements EasyNPC<Mob>, VariantDataCapable<Mob> {

    private String variantType = HumanoidSkinVariant.STEVE.name();
    private int npcDataVersion;

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getSynchedEntityData(SynchedDataIndex synchedDataIndex) {
      return synchedDataIndex == SynchedDataIndex.VARIANT_TYPE ? (T) this.variantType : null;
    }

    @Override
    public <T> void setSynchedEntityData(
        SynchedDataIndex synchedDataIndex, T data, boolean forceUpdate) {
      if (synchedDataIndex == SynchedDataIndex.VARIANT_TYPE) {
        this.variantType = String.valueOf(data);
      }
    }

    @Override
    public <T> void defineSynchedEntityData(SynchedDataIndex synchedDataIndex, T defaultData) {}

    @Override
    public int getNPCDataVersion() {
      return this.npcDataVersion;
    }

    @Override
    public void setNPCDataVersion(int version) {
      this.npcDataVersion = version;
    }

    @Override
    public FakePlayer getFakePlayer(ServerLevel level, BlockPos blockPos) {
      return null;
    }

    @Override
    public GoalSelector getEntityGoalSelector() {
      return null;
    }

    @Override
    public GoalSelector getEntityTargetSelector() {
      return null;
    }
  }
}
