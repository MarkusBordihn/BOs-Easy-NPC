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

package de.markusbordihn.easynpc.handler;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import de.markusbordihn.easynpc.compat.easymodelentities.EasyModelEntitiesManager;
import de.markusbordihn.easynpc.data.configuration.ConfigurationData;
import de.markusbordihn.easynpc.data.render.RenderDataEntry;
import de.markusbordihn.easynpc.data.render.RenderType;
import de.markusbordihn.easynpc.data.synched.SynchedDataIndex;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ConfigurationDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.RenderDataCapable;
import de.markusbordihn.easynpc.server.player.FakePlayer;
import java.util.EnumMap;
import net.minecraft.SharedConstants;
import net.minecraft.core.BlockPos;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.server.Bootstrap;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.goal.GoalSelector;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

class RenderHandlerTest {

  @BeforeAll
  static void bootstrap() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
  }

  @Test
  void setRenderEntityModelKeepsEasyModelEntityTypeForEasyModelNPC() {
    TestRenderNPC npc = new TestRenderNPC(ConfigurationData.EASY_MODEL);

    assertTrue(RenderHandler.setRenderEntityModel(npc, "my_pack:my_model"));
    assertEquals(RenderType.EASY_MODEL_ENTITY, npc.getRenderDataEntry().getRenderType());
    assertEquals("my_pack:my_model", npc.getRenderDataEntry().getRenderEntityModel());
  }

  @Test
  void setRenderEntityModelKeepsEasyModelEntityTypeForHumanoidEasyModelNPC() {
    TestRenderNPC npc = new TestRenderNPC(ConfigurationData.EASY_MODEL_HUMANOID);

    assertTrue(RenderHandler.setRenderEntityModel(npc, "my_pack:my_model"));
    assertEquals(RenderType.EASY_MODEL_ENTITY, npc.getRenderDataEntry().getRenderType());
    assertEquals("my_pack:my_model", npc.getRenderDataEntry().getRenderEntityModel());
  }

  @Test
  void setRenderEntityModelUsesCobblemonEntityTypeForCobblemonNPC() {
    TestRenderNPC npc = new TestRenderNPC(ConfigurationData.COBBLEMON);

    assertTrue(RenderHandler.setRenderEntityModel(npc, "cobblemon:pikachu"));
    assertEquals(RenderType.COBBLEMON_ENTITY, npc.getRenderDataEntry().getRenderType());
  }

  private static final class TestRenderNPC
      implements EasyNPC<Mob>, RenderDataCapable<Mob>, ConfigurationDataCapable<Mob> {

    private final EnumMap<SynchedDataIndex, Object> synchedData =
        new EnumMap<>(SynchedDataIndex.class);
    private final ConfigurationData configurationData;
    private int npcDataVersion;

    private TestRenderNPC(ConfigurationData configurationData) {
      this.configurationData = configurationData;
      this.synchedData.putIfAbsent(
          SynchedDataIndex.RENDER_DATA,
          new RenderDataEntry(
              RenderType.EASY_MODEL_ENTITY, null, EasyModelEntitiesManager.DEFAULT_PROFILE));
    }

    @Override
    public ConfigurationData getConfigurationData() {
      return this.configurationData;
    }

    @Override
    public int getNPCDataVersion() {
      return this.npcDataVersion;
    }

    @Override
    public void setNPCDataVersion(int version) {
      this.npcDataVersion = version;
    }

    @Override
    public boolean isServerSideInstance() {
      return true;
    }

    @Override
    public FakePlayer getFakePlayer(ServerLevel level, BlockPos blockPos) {
      return null;
    }

    @Override
    public <T> void defineSynchedEntityData(
        SynchedEntityData.Builder builder, SynchedDataIndex synchedDataIndex, T defaultData) {
      this.synchedData.putIfAbsent(synchedDataIndex, defaultData);
    }

    @Override
    public <T> void setSynchedEntityData(
        SynchedDataIndex synchedDataIndex, T data, boolean forceUpdate) {
      this.synchedData.put(synchedDataIndex, data);
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getSynchedEntityData(SynchedDataIndex synchedDataIndex) {
      return (T) this.synchedData.get(synchedDataIndex);
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
