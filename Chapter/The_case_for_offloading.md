# The Case for Offloading {#cha:case_for_offloading}
<!-- Go through [@Golkarifard2017], [@Jiao2013] and [@Nabi2015]. -->
Offloading of mobile code, or _remote execution_, is not a new idea at all, and has been around for some time, yet it has never really gained widespread traction in mainstream programming. Most mobile applications still mainly use servers for communicating with databases and other services that require centralization, such as social networks, etc. Even though the literature on mobile offloading has consistently shown the benefits of offloading, it has still not had a major impact in mainstream development---perhaps existing solutions have simply not been general and flexible enough to satisfy the requirements of most developers, and this is the cause of the low adoption.

In this chapter, we will show that there is indeed still a place for mobile offloading, a collection of cases of how earlier works have saved energy with offloading, along with a gathering of the literature, answering under what conditions offloading would be favorable.


## Saving Energy with Offloading {#sec:case_energy}
<!-- Make an overview of the different systems from \cite{cha:related_work} showing their improvement in energy consumption. Also take a look at [@Gupta2013], [@Vallina-rodriguez2012] -->
Arguably, the main objective of any offloading system has chiefly been to lower energy consumption on the mobile device itself. A secondary effect of this is that often times it would also increase performance of applications, by running the computations on stronger machines in the cloud. A lot of factors go into what impacts energy consumption, such as execution time, latency, network conditions, bandwidth, packet drops, etc.

In [@Nabi2015], the authors present a formative study on the effects of mobile computation, trying to answer several questions, such as what impact input size, bandwidth and network conditions have on the performance of offloading. The results clearly indicate show a gain in both performance and energy conservation, when offloading on fast networks (e.g. WiFi and LTE), while the case for slower networks were not as clear cut, and depends on the complexity of the task being offloaded.

For example, \gls{ocr} is a commonly used application used to evaluate the performance of an offloading system, because it is a moderately compute intensive task, that also highly depends on the complexity of the input it gets, not only on the size of the image. As such, with enough bandwidth transfer and low enough latency, as is the case usually with WiFi and LTE, one can get energy savings by offloading it to the cloud. On the other hand, if the connection is poor or bandwidth low, like on 3G, a image that is low in complexity is likely to incur a penalty if offloaded. We will look more at these considerations in [@sec:case_when], about when to offload, and under what conditions.


### Comparison of Previous Systems
Let us take a look at some of the concrete energy improvements that have been achieved in existing systems. We will take a closer look at all of these systems in \cref{cha:related_work}---for now, we will simply refer to them by their name.

\ \

Since each evaluation has been done in different manners, we try to relativize them, by summing them up to a range of their average results. For example, MAUI tests for four different \gls{rtt} times on WiFi, one on 3G and on local. This will be summed up as local = 100% of energy consumption, WiFi = 10%-14% of energy consumption, and 3G = 25%. Comparing to CloneCloud for example, which only tests with one \gls{rtt} on WiFi, one on 3G and local. We will take a maximum of two test cases from each evaluation. Additionally, since local execution is the base line (i.e. always 100%), it has been changed to show the Joule measurement of the test, or battery percentage drain (i.e. "6%" means that 6% of the battery has drained), for which the other values are relative to. With this methodology, we have constructed the table, shown in [@tbl:case_concrete_measurements].


-----------------------------------------------------------------------------------------
                      **Local**    **WiFi**     **3G**      **Test**
--------------------- ------------ ------------ ----------- -----------------------------
**MAUI**              28J          10--14%          25%     Facial Recognition

                      50J          66--80%          110%    Video Game

**CloneCloud**        155J         22%              43%     Image Search

                      40J          50%              125%    Behaviour Profiling

**UpShift**           6% drain     66%              N/A     Handwriting (100 iterations)

                      12% drain    58%              N/A     Handwriting (200 iterations)

**MobiCOP**           173J         9%               16%     Pure Java Computation

                      1332J        13%              19%     Large Input/Output
-----------------------------------------------------------------------------------------

Table: Comparison of results across existing offloading systems {#tbl:case_concrete_measurements}

Admittedly, this can seem a bit confusing initially, but the point still manages to come across---there are significant gains to be achieved by offloading code. These gains primarily show themselves in the form of shorter execution time, which in turn means that the CPU needs to stay active less time, and also opens the opportunity for the screen needing to be on for less time, saving further energy.

For an overview of the values used in [@tbl:case_concrete_measurements], check out \cref{app:comparison_of_results}, which collects the measurements from the various papers on these systems.


## Where and When to offload {#sec:case_when}
<!-- Take a look at [@Golkarifard2017], [@Jiao2013] and [@Nabi2015]. Additionally [@Huang2012] might be interesting. -->
Now that we have established that offloading has the ability to give significant savings in energy consumption, and also improve execution time, we will take a look at when exactly one wants to offload.

In [@Golkarifard2017], the authors aim to establish a set of guidelines and recommendations, based on a review of the literature along with their own experiences. The paper is fairly recent, so this gives us some hope that it is up-to-date with current technologies and practices.

The main network factors impacting offloading are:

- Latency, and by extension \gls{rtt}.
- Bandwidth to the remote execution server.
- Packet loss ratio along with number of packets transmitted.
- Signal strength, although this is closely related to packet loss ratio.
- Network type, e.g. Bluetooth, 3G, WiFi, 4G. The energy required to transmit differs on each of these components, with WiFi requiring the lowest amount of energy to transmit.

For the application side, we mainly should consider:

- Execution time of a method.
- Input and output size.

As for what methods that have high potential for offloading, we have:

- CPU/Compute intensive operations, with relatively small input/output size. Some examples of these include (not taking into account I/O size) \gls{ocr}, \gls{ml}, image processing, video encoding/decoding, graph related computations, such as path algorithms in games, shortest route in maps, etc.
- Parallelizable tasks that are data independent.
- Methods requiring large amounts of external data transfer, such as downloading an Internet site and processing it, or making a lot of database calls for a computation.


### Where to Offload
A crucial aspect to offloading is the latency to the remote server that the code should be run on, and therefore location matters. [@Golkarifard2017] divides these into three main categories:

- Cloud: Remote server connected via the Internet.
- Cloudlet: Server located on the same WiFi.
- Ambient Cloud: nearby devices, such as smartphones, tablets, and wearables.

Each have their own tradeoffs. The ambient cloud usually does not present itself with much computational power, but can be used for small parallelizable tasks, that can be distributed to a lot of devices. Cloudlets offer the superior power of servers like the cloud, at a lower latency, but comes at a significant cost to infrastructure, and limits the pervasiveness of offloading, since it is limited to the networks the cloudlets are placed on. Finally, clouds offer the most flexibility with regards to infrastructure and power, but obviously comes at the cost of being further away from the mobile device, and therefore incurs a higher latency cost.

The only consideration really impacted by the location of the remote execution server is latency and \gls{rtt}. It will certainly affect the rest of the parameters we look at, but only since it is part of the equation to calculate the impact of these. In this thesis we will mostly be focusing on a cloud environment, but there is nothing stopping the approach from being used on the other two---cloudlets are basically interchangeable with cloud servers, but ambient clouds would require special setup, and accompanying clients on each device.


### Network Conditions {#sec:case_when_network}
<!-- This goes back to [@Golkarifard2017], [@Jiao2013] and [@Nabi2015] again. -->
The bulk of our considerations stem from network related considerations; the network type, bandwidth, packet loss ratio and signal strength.

We can quickly dismiss using Bluetooth for offloading. In [@Golkarifard2017] they give an example of offloading a 300 KB image in 0.1 second, which would require a 24 Mb/s connection to achieve ($(300\text{kB} * 8 \frac{\text{kb}}{\text{kB}}) / 0.1\text{s} = 24\text{Mb/s}$). This would saturate the maximum throughput of Bluetooth version 4.0 (at 25Mb/s), according to table 1 on page 3 in the paper. On the same page, table 2, we note that WiFi versions coming after 802.11b, that is 802.11b, n and ac, have speeds at 54Mb/s, 150Mb/s and 411Mb/s respectively.

In [@Huang2012] the authors give a thorough comparison of the different radio technologies, and their energy per bit transferred. For reference, their results are included here in [@fig:case_network_energy_network].

![Energy per Bit Transferred [@Huang2012 p.9]](Graphic/EnergyPerBit.png "Energy per Bit Transferred"){#fig:case_network_energy_network width=100% }

We see that LTE downlink compares well with WiFi up- and downlink, and LTE uplink is slightly more expensive to perform, especially in lower transfer sizes, but is amortized as the data size increases. 3G compares considerably worse to both, especially in the uplink. From this, we can get an intuition of what network conditions are favorable to offloading.

For concrete results, we go back to [@Nabi2015]


### Input/Output Size {#sec:case_when_input}
<!-- Again we go back to [@Golkarifard2017], [@Jiao2013] and [@Nabi2015] again. -->
Another major factor we have to take into consideration is the input/output size that we will be transferring. These are highly affected by both throughput and \gls{rtt}. In [@Cuervo2010], the authors measure the energy consumed by uploading 10KB and 100KB of code over different \gls{rtt} values. For reference, their measurements are included here in [@fig:case_network_energy_rtt].

![Energy consumed by offloading 10KB and 100KB over different RTT values [@Cuervo2010 p.3]](Graphic/EnergyToRTT.png "Energy consumed by offloading 10KB and 100KB over different RTT values"){#fig:case_network_energy_rtt width=100% }

We see that the the energy consumption increases more quickly with larger data sizes, as the \gls{rtt} goes up. This fits naturally with our intuition that the more packets that are sent, the more ACKs must be responded with, increasing the impact of \gls{rtt}---at least over \gls{tcp}. As such, we must clearly combine our information about the current \gls{rtt} values we are getting, with the data size that we want to offload, and how much impact the specific function has on energy consumption.


### Other Considerations
[@Golkarifard2017] also mentions creating a \gls{vm} manager to automatically create \glspl{vm} as needed. This process can be entirely automated by most \gls{iaas} providers, such as with \gls{aws} OpsWorks, which allow easy scaling based on metrics, such as CPU load on the servers, time, and much more. For example, one server could be running in during low traffic times, such as during the night, and four additional servers could then be spun up during the day, where load is estimated to be much higher. One could also automatically scale up the number of \gls{vm}, by setting a rule that if a server is utilizing over 90% of CPU for more than 5 minutes, then spin up two new instances. If then the CPU utilization is under 40% for 10 minutes, then remove one instance, repeating until reaching a minimum set of instances.


## Summary
We have demonstrated that there indeed is a clear case for offloading of computations still, even though widespread adoption might still be low. The effectiveness of this has been backed up by concrete measurements from previous systems.

Furthermore, we have layed out some which factors must be considered when designing an effective offloading system, based on literature review, along with a more detailed reasoning as to how some of these factors can affect the system performance. For example, it is clear to us that if we have a good \gls{rtt} to the remote execution server, and satisfactory throughput, such as on WiFi or LTE, then it is almost always beneficial to offload. The situation becomes much more clouded on networks with lower throughput and worse latency, such as 3G and 2G, and here input size makes a much bigger difference, and must be taken into account.
