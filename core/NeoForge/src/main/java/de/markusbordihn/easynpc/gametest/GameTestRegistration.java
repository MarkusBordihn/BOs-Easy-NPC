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

package de.markusbordihn.easynpc.gametest;

import de.markusbordihn.easynpc.Constants;
import java.util.Collection;
import net.minecraft.gametest.framework.GameTestGenerator;
import net.minecraft.gametest.framework.TestFunction;
import net.neoforged.fml.ModList;
import net.neoforged.neoforge.gametest.GameTestHolder;

@GameTestHolder(Constants.MOD_ID)
public class GameTestRegistration {

  @GameTestGenerator
  public Collection<TestFunction> gameTests() {
    return GameTestFunctions.create(
        Constants.MOD_ID,
        ModList.get()::isLoaded,
        SmokeTest.class,
        BackupTest.class,
        DialogScreenTest.class,
        ModSpawnEggItemTest.class,
        ConditionEvaluationTest.class,
        VillagerVariantTest.class,
        FactionObjectiveTest.class,
        ObjectiveGoalCreationTest.class,
        ExecuteAsUserCommandTest.class,
        PresetCommandSecurityTest.class,
        OwnerCommandTest.class,
        MoveEasyNPCItemTest.class,
        VisibilityAttributeTest.class,
        EnvironmentalAttributeTest.class,
        FactionDefenseTest.class,
        IllagerCrossedArmsTest.class,
        NPCEntityLifecycleTest.class,
        ShoulderTest.class,
        NPCMassLifecycleTest.class,
        NPCDataIsolationTest.class,
        TradingPersistenceTest.class,
        NpcStateTest.class,
        DialogButtonIdentityTest.class,
        ObjectivePresetTest.class,
        CommandArgumentTypeTest.class,
        NavigationTypeTest.class,
        DistanceActionTest.class,
        IntervalActionAudienceTest.class,
        PresetRoundTripTest.class,
        PresetIdentityTest.class,
        PresetDefaultBaselineTest.class,
        BasePresetTest.class,
        DefaultPresetRoundTripTest.class,
        StoredDataTest.class,
        PresetSpawnTest.class,
        TradingActionTest.class,
        WaitActionTest.class,
        SoundActionTest.class,
        PauseTest.class,
        PlayerIdleTest.class,
        ImmovableAttributeTest.class,
        FallbackActionTest.class,
        MoveActionTest.class,
        OpacityAttributeTest.class,
        BossBarTest.class,
        InventoryTest.class,
        WaterStateAccessTest.class,
        RenderEntityTypeTest.class,
        ObjectiveApiTest.class,
        AssistOwnerTest.class,
        InteractionResultTest.class,
        EasyModelEntitiesTest.class);
  }
}
